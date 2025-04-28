mod history;
mod movepick;

pub use movepick::MovePicker;

use crate::eval::{EVAL_ZERO, Eval};
use chess::{
    Move,
    board::{Board, CaptureGen, MoveList, QuietGen},
};

pub enum MoveStage {
    TTMove,
    GenCaptures,
    GoodCaptures,
    GenQuiets,
    Quiets,
    BadCaptures,
}

pub struct ScoredMoveList {
    move_list: MoveList,
    scores: [Eval; 256],
    index: usize,
}

impl ScoredMoveList {
    pub fn new() -> Self {
        Self {
            move_list: MoveList::new(),
            scores: [EVAL_ZERO; 256],
            index: 0,
        }
    }

    pub fn generate_captures(&mut self, board: &Board) {
        board.generate_moves::<CaptureGen>(&mut self.move_list);
    }

    pub fn generate_quiets(&mut self, board: &Board) {
        board.generate_moves::<QuietGen>(&mut self.move_list);
    }

    pub fn partial_sort(&mut self, start: usize, end: usize, threshold: Eval) {
        for i in start..end {
            if self.scores[i] < threshold {
                continue;
            }

            let mut j = i;
            let move_ = self.move_list[j];
            let score = self.scores[j];

            while j > start && self.scores[j - 1] < self.scores[j] {
                self.move_list.swap(j, j - 1);
                self.scores.swap(j, j - 1);
                j -= 1;
            }

            self.scores[j] = score;
            self.move_list[j] = move_;
        }
    }

    pub fn set_index(&mut self, index: usize) {
        self.index = index;
    }

    pub fn next(&mut self, end: usize) -> Option<Move> {
        if self.index >= end {
            return None;
        }
        let move_ = self.move_list[self.index];
        self.index += 1;
        Some(move_)
    }

    pub fn len(&self) -> usize {
        self.move_list.len()
    }
}
