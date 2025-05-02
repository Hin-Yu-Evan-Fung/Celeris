mod history;
mod movepick;

pub use history::{History, KillerTable, MainHistory};
pub use movepick::MovePicker;

use crate::{eval::Eval, search::SearchStats};
use chess::{
    Move,
    board::{Board, CaptureGen, MoveList, QuietGen},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MoveStage {
    TTMove,
    GenCaptures,
    GoodCaptures,
    GenQuiets,
    Quiets,
    BadCaptures,
}

// pub struct ScoredMoveList {
//     move_list: MoveList,
//     scores: [Eval; 256],
//     index: usize,
// }

// impl ScoredMoveList {
//     pub fn new() -> Self {
//         Self {
//             move_list: MoveList::new(),
//             scores: [Eval::ZERO; 256],
//             index: 0,
//         }
//     }

//     pub fn gen_captures(&mut self, board: &Board) {
//         board.generate_moves::<CaptureGen>(&mut self.move_list);
//     }

//     pub fn gen_quiets(&mut self, board: &Board) {
//         board.generate_moves::<QuietGen>(&mut self.move_list);
//     }

//     pub fn partial_sort(&mut self, start: usize, end: usize) {
//         for i in start..end {
//             let mut j = i;
//             let move_ = self.move_list[j];
//             let score = self.scores[j];

//             while j > start && self.scores[j - 1] < self.scores[j] {
//                 self.move_list.swap(j, j - 1);
//                 self.scores.swap(j, j - 1);
//                 j -= 1;
//             }

//             self.scores[j] = score;
//             self.move_list[j] = move_;
//         }
//     }

//     pub fn set_index(&mut self, index: usize) {
//         self.index = index;
//     }

//     pub fn set_score(&mut self, index: usize, score: Eval) {
//         self.scores[index] = score;
//     }

//     pub fn next(&mut self, end: usize) -> Option<Move> {
//         if self.index >= end {
//             return None;
//         }
//         let move_ = self.move_list[self.index];
//         self.index += 1;
//         Some(move_)
//     }

//     pub fn len(&self) -> usize {
//         self.move_list.len()
//     }
// }
