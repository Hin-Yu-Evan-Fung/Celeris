use crate::tunables::*;

use super::Eval;
use super::psqt::{calc_game_phase, calc_psqt};
use chess::{Colour, PieceType, board::Board};
use nnue::accummulator::Accumulator;

pub fn evaluate(board: &Board) -> Eval {
    let score = calc_psqt(board);

    let (mg_phase, eg_phase) = calc_game_phase(board);

    let weighted_mg = (score.0.0 as i64) * (mg_phase as i64);
    let weighted_eg = (score.1.0 as i64) * (eg_phase as i64);

    let eval = (weighted_mg + weighted_eg) / 24;

    let v = Eval(eval.clamp(-Eval::MATE.0 as i64, Eval::MATE.0 as i64) as i16);

    if board.stm() == Colour::White { v } else { -v }
}

#[rustfmt::skip]
 pub fn evaluate_nnue(board: &Board, nnue: &mut Accumulator) -> Eval {
     // nnue output
     let mut v = nnue.evaluate(board);
 
     let material_scale = (
         pawn_val()   * board.piecetype_bb(PieceType::Pawn).count_bits()   as i32 +
         knight_val() * board.piecetype_bb(PieceType::Knight).count_bits() as i32 +
         bishop_val() * board.piecetype_bb(PieceType::Bishop).count_bits() as i32 +
         rook_val()   * board.piecetype_bb(PieceType::Rook).count_bits()   as i32 +
         queen_val()  * board.piecetype_bb(PieceType::Queen).count_bits()  as i32
     ) / 32;
 
     v = (v * (nnue_base() + material_scale)) / 1024;
 
     Eval(v as i16)
 }
