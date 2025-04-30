use chess::Colour;
use chess::Board;

use super::Eval;

use super::psqt::{calc_game_phase, calc_psqt};

pub fn evaluate(board: &Board) -> Eval {
    let score = calc_psqt(board);

    let (mg_phase, eg_phase) = calc_game_phase(board);

    let weighted_mg = (score.0.0 as i64) * (mg_phase as i64);
    let weighted_eg = (score.1.0 as i64) * (eg_phase as i64);

    let v = Eval(((weighted_mg + weighted_eg) / 24) as i16);

    if board.stm() == Colour::White { v } else { -v }
}
