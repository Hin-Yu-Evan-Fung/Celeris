use chess::Move;

use crate::{MAX_DEPTH, eval::Eval};

pub struct HistoryEntry<T, const MAX_HISTORY: i16> {
    entry: T,
}

impl<T, const MAX_HISTORY: i16> HistoryEntry<T, MAX_HISTORY> {
    /// # Returns a reference to the entry value
    pub fn get(&self) -> &T {
        &self.entry
    }
}

impl<const MAX_HISTORY: i16> HistoryEntry<Eval, MAX_HISTORY> {
    /// # Updates history
    ///
    /// ### History gravity formula
    /// - https://www.chessprogramming.org/History_Heuristic
    ///
    /// - Scales up history when a beta cutoff is unexpected, and scales down history when a beta cutoff is expected
    /// - Keeps the history values clamped between MAX and -MAX
    pub fn update(&mut self, bonus: i16) {
        let bonus = bonus.clamp(-MAX_HISTORY, MAX_HISTORY);
        self.entry += Eval(bonus - self.entry.0 * bonus.abs() / MAX_HISTORY as i16);
    }
}

pub const NOT_USED: i16 = 0;

pub type KillerEntry = HistoryEntry<Move, NOT_USED>;

pub struct KillerTable {
    killers: [[KillerEntry; 2]; MAX_DEPTH],
}

impl KillerTable {}
