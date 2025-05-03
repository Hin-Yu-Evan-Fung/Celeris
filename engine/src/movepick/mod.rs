mod history;
mod movepick;
mod see;

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
    Killer1,
    Killer2,
    GenQuiets,
    Quiets,
    BadCaptures,
}
