use std::mem::MaybeUninit; // Import MaybeUninit
use std::slice; // Import slice for iter()

use crate::Move;

const MAX_LEGAL_MOVES: usize = 256;

pub struct MoveList {
    // Array of individually uninitialized Moves
    moves: [MaybeUninit<Move>; MAX_LEGAL_MOVES],
    num_moves: usize,
}

impl std::ops::Index<usize> for MoveList {
    type Output = Move;

    fn index(&self, index: usize) -> &Self::Output {
        // Bounds check is crucial for safety here.
        debug_assert!(index < self.num_moves, "MoveList index out of bounds");

        // Safety:
        // 1. We asserted `index < self.num_moves`, ensuring the element at `index` was initialized.
        // 2. `assume_init_read()` is safe because the invariant guarantees initialization.
        unsafe { self.moves[index].assume_init_ref() }
    }
}

impl std::ops::IndexMut<usize> for MoveList {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        // Bounds check is crucial for safety here.
        debug_assert!(index < self.num_moves, "MoveList index out of bounds");

        // Safety:
        // 1. We asserted `index < self.num_moves`, ensuring the element at `index` was initialized.
        // 2. `assume_init_read()` is safe because the invariant guarantees initialization.
        unsafe { self.moves[index].assume_init_mut() }
    }
}

// Safety invariant: `num_moves` accurately tracks the number of initialized
// elements in the `moves` array. Any index `< num_moves` corresponds to an
// element where `.write()` has been called, making it safe to `assume_init*`.
// Any index `>= num_moves` is potentially uninitialized.

impl MoveList {
    #[inline]
    pub fn new() -> MoveList {
        MoveList {
            // Create an array of MaybeUninit<Move>. Since MaybeUninit<T> is Copy
            // (if T doesn't impl Drop, which Move likely doesn't), this syntax works
            // and efficiently allocates the space without initializing the underlying Moves.
            moves: [MaybeUninit::uninit(); MAX_LEGAL_MOVES],
            num_moves: 0,
        }
    }

    #[inline]
    pub(super) fn add_move(&mut self, move_: Move) {
        // Bounds check
        debug_assert!(self.num_moves < MAX_LEGAL_MOVES);

        // Safety: This is now SAFE Rust.
        // We access the `MaybeUninit<Move>` at the current index and write
        // the `move_` into it, marking it as initialized.
        self.moves[self.num_moves].write(move_);

        // Increment count *after* writing.
        self.num_moves += 1;
    }

    #[inline]
    pub(super) fn pop_move(&mut self) -> Move {
        // Bounds check
        debug_assert!(self.num_moves > 0);

        // Decrement count *before* reading.
        self.num_moves -= 1;

        // Safety:
        // 1. We decremented `num_moves`, so it now refers to the index of the last *initialized* element.
        // 2. The invariant guarantees that `self.moves[self.num_moves]` has been initialized via `write`.
        // 3. `assume_init_read()` safely reads the value *assuming* it's initialized.
        //    (Assumes `Move` is `Copy`. If not, this might leak, but `Move` is likely `Copy`).
        unsafe { self.moves[self.num_moves].assume_init_read() }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.num_moves
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.num_moves == 0
    }

    #[inline]
    pub fn swap(&mut self, index1: usize, index2: usize) {
        self.moves.swap(index1, index2);
    }

    #[inline]
    pub fn get_num_moves(&self) -> usize {
        self.num_moves
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, Move> {
        // Safety:
        // 1. `MaybeUninit<T>` is guaranteed to have the same layout as `T`.
        // 2. `self.moves.as_ptr()` gives a pointer to the first `MaybeUninit<Move>`.
        // 3. Casting `*const MaybeUninit<Move>` to `*const Move` is safe due to the layout guarantee.
        // 4. The invariant guarantees that the first `self.num_moves` elements have been initialized.
        // 5. Therefore, creating a `&[Move]` slice of length `self.num_moves` from this pointer
        //    points to valid, initialized `Move` data.
        unsafe {
            let ptr = self.moves.as_ptr() as *const Move;
            slice::from_raw_parts(ptr, self.num_moves)
        }
        .iter()
    }

    #[inline]
    pub fn for_each(&self, mut f: impl FnMut(&Move)) {
        // Can simply use the safe iterator we just defined.
        for move_ in self.iter() {
            f(move_);
        }
    }
}
