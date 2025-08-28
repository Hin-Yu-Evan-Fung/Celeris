use chess::{Bitboard, Colour, PieceType, board::Board};

use crate::{
    flatten::flatten,
    params::{L1, NNUE_EMBEDDED, QA, QAB, SCALE},
    utils::{Align64, feature_index},
};

pub type SideAccumulator = Align64<[i16; L1]>;

#[derive(Clone, Copy, Debug)]
pub struct Accumulator {
    pub white: SideAccumulator,
    pub black: SideAccumulator,

    c_bb: [Bitboard; Colour::NUM],
    p_bb: [Bitboard; PieceType::NUM],
}

impl Default for Accumulator {
    fn default() -> Self {
        Self {
            white: NNUE_EMBEDDED.feature_bias,
            black: NNUE_EMBEDDED.feature_bias,
            c_bb: [Bitboard::EMPTY; Colour::NUM],
            p_bb: [Bitboard::EMPTY; PieceType::NUM],
        }
    }
}

pub const ON: bool = true;
pub const OFF: bool = false;

impl Accumulator {
    #[inline(never)]
    fn update_weights<const ON: bool>(&mut self, ft: (usize, usize)) {
        let update = |acc: &mut SideAccumulator, idx: usize| {
            debug_assert!(idx + L1 <= NNUE_EMBEDDED.feature_weights.len());
            acc.iter_mut()
                .zip(&NNUE_EMBEDDED.feature_weights[idx..idx + L1])
                .for_each(|(acc_val, &weight)| {
                    *acc_val += if ON { weight } else { -weight };
                });
        };

        update(&mut self.white, ft.0);
        update(&mut self.black, ft.1);
    }

    pub fn update(&mut self, board: &Board) {
        let wksq = board.ksq(Colour::White);
        let bksq = board.ksq(Colour::Black);

        for c in 0..Colour::NUM {
            let old_c = self.c_bb[c];
            let new_c = board.occupied[c];

            for p in 0..PieceType::NUM {
                let old_pc = old_c & self.p_bb[p];
                let new_pc = new_c & board.pieces[p];

                (new_pc & !old_pc).for_each(|s| {
                    let ft = feature_index(c, p, wksq, bksq, s.index());
                    self.update_weights::<ON>(ft);
                });

                (old_pc & !new_pc).for_each(|s| {
                    let ft = feature_index(c, p, wksq, bksq, s.index());
                    self.update_weights::<OFF>(ft);
                });
            }
        }

        self.c_bb = board.occupied;
        self.p_bb = board.pieces;
    }
}

#[inline]
pub fn screlu(x: i16) -> i32 {
    (x.clamp(0, QA as i16) as i32).pow(2)
}

impl Accumulator {
    pub fn propagate(&self, c: Colour, output_bucket: usize) -> i32 {
        let (stm, opp) = match c {
            Colour::White => (self.white, self.black),
            Colour::Black => (self.black, self.white),
        };

        let weights = NNUE_EMBEDDED.output_weights;
        let sum =
            flatten(&stm, &weights[output_bucket][0]) + flatten(&opp, &weights[output_bucket][1]);
        (sum / QA + NNUE_EMBEDDED.output_bias[output_bucket] as i32) * SCALE / QAB
    }
}
