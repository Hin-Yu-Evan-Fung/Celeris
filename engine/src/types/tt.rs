use std::{
    cmp,
    mem::size_of,
    sync::atomic::{AtomicU64, Ordering},
    thread::Thread,
};

use chess::Move;

use crate::{Eval, thread::ThreadPool};

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Default)]
pub enum TTBound {
    #[default]
    Upper, // Fail low nodes
    Lower, // Fail-High nodes (Beta cutoff)
    Exact, // PV nodes
    None,  // Just writing to tt
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub struct TTEntry {
    key: u64,        // 64 bits
    age: u8,         //  7 bits
    depth: u8,       //  7 bits
    bound: TTBound,  //  2 bits
    best_move: Move, // 16 bits
    eval: Eval,      // 16 bits - truncated
    value: Eval,     // 16 bits - truncated
}

impl TTEntry {
    pub const AGE_MASK: u64 = 0x7F;
    pub const DEPTH_MASK: u64 = 0x7F << 7;
    pub const BOUND_MASK: u64 = 0x3 << 14;
    pub const MOVE_MASK: u64 = 0xFFFF << 16;
    pub const EVAL_MASK: u64 = 0xFFFF << 32;
    pub const VALUE_MASK: u64 = 0xFFFF << 48;

    pub fn unpack_age(data: u64) -> u8 {
        (data & Self::AGE_MASK) as u8
    }

    pub fn unpack_depth(data: u64) -> u8 {
        ((data & Self::DEPTH_MASK) >> 7) as u8
    }

    pub fn unpack_bound(data: u64) -> TTBound {
        match (data & Self::BOUND_MASK) >> 14 {
            0 => TTBound::Upper,
            1 => TTBound::Lower,
            2 => TTBound::Exact,
            _ => TTBound::None,
        }
    }

    pub fn unpack_move(data: u64) -> Move {
        unsafe { Move::new_raw(((data & Self::MOVE_MASK) >> 16) as u16) }
    }

    pub fn unpack_eval(data: u64) -> Eval {
        Eval(((data & Self::EVAL_MASK) >> 32) as i32)
    }

    pub fn unpack_value(data: u64) -> Eval {
        Eval(((data & Self::VALUE_MASK) >> 48) as i32)
    }

    pub(super) fn pack(&self) -> PackedTTEntry {
        let mut data = 0;
        data |= (self.age as u64) & TTEntry::AGE_MASK;
        data |= ((self.depth as u64) << 7) & TTEntry::DEPTH_MASK;
        data |= ((self.bound as u64) << 14) & TTEntry::BOUND_MASK;
        data |= ((self.best_move.raw() as u64) << 16) & TTEntry::MOVE_MASK;
        data |= ((self.eval.0 as u64) << 32) & TTEntry::EVAL_MASK;
        data |= ((self.value.0 as u64) << 48) & TTEntry::VALUE_MASK;
        PackedTTEntry {
            key: AtomicU64::new(self.key),
            data: AtomicU64::new(data ^ self.key),
        }
    }
}

impl From<TTEntry> for PackedTTEntry {
    fn from(entry: TTEntry) -> Self {
        entry.pack()
    }
}

impl From<(u64, u64)> for TTEntry {
    fn from((key, data): (u64, u64)) -> Self {
        Self {
            key,
            age: Self::unpack_age(data),
            depth: Self::unpack_depth(data),
            bound: Self::unpack_bound(data),
            best_move: Self::unpack_move(data),
            eval: Self::unpack_eval(data),
            value: Self::unpack_value(data),
        }
    }
}

// The entry stored in the TT itself.
// This is the full compressed TT value, stored in 64 bits.
// The extra 64 bits are a checksum, where we can make sure that the key
// is valid and has not been modified by two threads simultaneously.

/// Packed TT key
///
/// ## Packing Scheme
///
/// | Field     | Bits | Offset |
/// | --------- | ---- | ------ |
/// | age       | 7    | 0      |
/// | depth     | 7    | 7      |
/// | bound     | 2    | 14     |
/// | best_move | 16   | 16     |
/// | eval      | 16   | 32     |
/// | value     | 16   | 48     |
///
/// ## Checksum
///
/// The checksum is calculated by XORing the key with the data.
///
/// ## Usage
///
/// The key is used to index into the TT.
/// The data is used to store the TT entry.
/// The checksum is used to verify that the key is valid.
///
/// ## Example
///
/// ```
/// use std::sync::atomic::{AtomicU64, Ordering};
/// use chess::Move;
/// use crate::types::tt::{TTBound, TTEntry};
///
/// let mut entry = TTEntry::default();
/// entry.age = 1;
/// entry.depth = 2;
/// entry.bound = TTBound::Exact;
/// entry.best_move = Move::default();
/// entry.eval = 100;
/// entry.value = 200;
/// entry.key = 12345;
///
// / let packed = entry.pack();
// / let unpacked = TTEntry::unpack(packed.key.load(Ordering::Relaxed), packed.data.load(Ordering::Relaxed));
///
/// assert_eq!(entry, unpacked);
///

#[derive(Debug, Default)]
struct PackedTTEntry {
    key: AtomicU64,
    data: AtomicU64,
}

impl PackedTTEntry {
    fn read(&self, pos_key: u64) -> Option<TTEntry> {
        let key = self.key.load(Ordering::Relaxed);
        let data = self.data.load(Ordering::Relaxed);

        if key ^ pos_key == data {
            Some((key, data).into())
        } else {
            None
        }
    }

    unsafe fn read_unchecked(&self) -> TTEntry {
        let key = self.key.load(Ordering::Relaxed);
        let data = self.data.load(Ordering::Relaxed);

        (key, data).into()
    }

    /// Clears the atomic values in this entry.
    #[inline]
    fn clear(&self) {
        // Use Relaxed ordering as we don't need synchronization between
        // clearing different entries, just atomicity for each store.
        self.key.store(0, Ordering::Relaxed);
        self.data.store(0, Ordering::Relaxed);
    }
}

/// Transposition table
pub struct TT {
    table: Vec<PackedTTEntry>,
    age: u8,
}

impl Default for TT {
    fn default() -> Self {
        let mut tt = Self {
            table: Vec::new(),
            age: 0,
        };

        tt.resize(Self::DEFAULT_SIZE);

        tt
    }
}

impl TT {
    pub const DEFAULT_SIZE: usize = 16;

    fn calc_no_of_entries(mb: usize) -> usize {
        (mb << 20) / size_of::<PackedTTEntry>()
    }

    /// Creates an empty hash table (Only used for tests)
    pub fn new() -> Self {
        Self {
            table: Vec::new(),
            age: 0,
        }
    }

    pub fn resize(&mut self, mb: usize) {
        self.table
            .resize_with(Self::calc_no_of_entries(mb), PackedTTEntry::default);
    }

    pub fn size(&self) -> usize {
        self.table.len()
    }

    pub fn get(&self, pos_key: u64) -> Option<TTEntry> {
        let key = pos_key as u128;
        let len = self.table.len() as u128;
        let index = (key * len) >> 64;
        self.table[index as usize].read(pos_key)
    }

    // --- Age methods need &mut self ---
    // These must be called when exclusive access is guaranteed (e.g., main thread, pool idle)
    pub fn increment_age(&mut self) {
        self.age = (self.age + 1) & TTEntry::AGE_MASK as u8;
    }
    pub fn current_age(&self) -> u8 {
        self.age
    }
}

impl ThreadPool {
    pub fn clear_hash_table(&mut self, tt: &TT) {
        let no_of_entries = tt.table.len();
        if no_of_entries == 0 {
            return;
        }

        let nums_threads = self.size();

        let chunk_size = no_of_entries / nums_threads; // Floor division

        std::thread::scope(|s| {
            for i in 0..nums_threads {
                let start = i * chunk_size;
                let end = if i == nums_threads - 1 {
                    no_of_entries
                } else {
                    (i + 1) * chunk_size
                };

                s.spawn(move || {
                    for j in start..end {
                        tt.table[j].clear();
                    }
                });
            }
        });
    }
}
