#![cfg_attr(
    all(target_arch = "x86_64", target_feature = "avx512f"),
    feature(stdarch_x86_avx512)
)]

use chess::{Colour, board::Board};

use crate::{
    accumulator::Accumulator,
    params::{NNUE_EMBEDDED, NUM_INPUT_BUCKETS, NUM_OUTPUT_BUCKETS},
    utils::{box_array, input_bucket_index, output_bucket_index},
};

pub mod accumulator;
pub mod flatten;
pub mod params;
pub mod utils;

#[derive(Clone, Debug)]
pub struct NNUE {
    table: Box<[[Accumulator; 2 * NUM_INPUT_BUCKETS]; 2 * NUM_INPUT_BUCKETS]>,
}

impl Default for NNUE {
    fn default() -> Self {
        let mut table: Box<[[Accumulator; 2 * NUM_INPUT_BUCKETS]; 2 * NUM_INPUT_BUCKETS]> =
            box_array();

        for x in table.iter_mut() {
            for y in x.iter_mut() {
                y.white = NNUE_EMBEDDED.feature_bias;
                y.black = NNUE_EMBEDDED.feature_bias;
            }
        }

        Self { table }
    }
}

impl NNUE {
    pub fn evaluate(&mut self, board: &Board) -> i32 {
        let wksq = board.ksq(Colour::White);
        let bksq = board.ksq(Colour::Black);

        let w_bucket = input_bucket_index(wksq, Colour::White);
        let b_bucket = input_bucket_index(bksq, Colour::Black);
        let out_bucket = output_bucket_index(board.all_occupied_bb().count_bits() as usize);

        let acc = &mut self.table[w_bucket][b_bucket];

        acc.update(board);
        acc.propagate(board.stm(), out_bucket)
    }
}
