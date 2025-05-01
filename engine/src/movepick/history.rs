use chess::Move;

use crate::eval::Eval;

pub struct HistoryEntry<T, const MAX: usize> {
    entry: T,
}

impl<T, const MAX: usize> HistoryEntry<T, MAX> {
    /// # Returns a reference to the entry value
    pub fn get(&self) -> &T {
        &self.entry
    }

    /// # Updates history
    ///
    /// ### History gravity formula
    /// - https://www.chessprogramming.org/History_Heuristic
    ///
    /// - Scales up history when a beta cutoff is unexpected, and scales down history when a beta cutoff is expected
    /// - Keeps the history values clamped between MAX and -MAX
    pub fn update(&self, bonus: Eval) {
        bonus = bonus.clamp(-MAX, MAX);
        self.entry += bonus - self.entry * abs(bonus) / MAX;
    }
}

pub const NOT_USED = 0;

pub struct KillerTable {
    killers: [[Self::Entry; 2]; MAX_DEPTH]
}

impl KillerTable {
    pub type Entry = HistoryEntry<Move, NOT_USED>;


}
