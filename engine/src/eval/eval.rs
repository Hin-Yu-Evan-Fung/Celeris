use chess::{Board, Colour};

use super::psqt::{calc_game_phase, calc_psqt};
use super::{Eval, PawnTable};
use crate::MATE;

pub fn evaluate(board: &Board, pawn_table: &mut PawnTable) -> Eval {
    let mut score = calc_psqt(board);

    let pawn_entry = &mut pawn_table.get(board);

    score += pawn_entry.scores[Colour::White as usize];
    score -= pawn_entry.scores[Colour::Black as usize];
    score += pawn_entry.king_safety(board, Colour::White);
    score -= pawn_entry.king_safety(board, Colour::Black);

    let (mg_phase, eg_phase) = calc_game_phase(board);

    let weighted_mg = (score.0.0 as i64) * (mg_phase as i64);
    let weighted_eg = (score.1.0 as i64) * (eg_phase as i64);

    let eval = (weighted_mg + weighted_eg) / 24;

    let v = Eval(eval.clamp(-MATE.0 as i64, MATE.0 as i64) as i16);

    if board.stm() == Colour::White { v } else { -v }
}
