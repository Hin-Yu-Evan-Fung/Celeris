use chess::Colour;
use chess::board::Board;

use super::Eval;

use super::psqt::{calc_game_phase, calc_psqt};

pub fn evaluate(board: &Board) -> Eval {
    let score = calc_psqt(board);

    let (mg_phase, eg_phase) = calc_game_phase(board);

    let v = Eval((score.0.0 * mg_phase + score.1.0 * eg_phase) / 24 as i16);

    if board.stm() == Colour::White { v } else { -v }
}
