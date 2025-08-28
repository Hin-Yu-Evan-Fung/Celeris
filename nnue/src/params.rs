use std::mem;

use crate::utils::Align64;

pub const INPUT: usize = 768;
pub const L1: usize = 512;

pub const NUM_INPUT_BUCKETS: usize = 4;
pub const NUM_OUTPUT_BUCKETS: usize = 8;

pub const QA: i32 = 255;
pub const QB: i32 = 64;
pub const QAB: i32 = QA * QB;
pub const SCALE: i32 = 400;

#[rustfmt::skip]
pub const BUCKET_MAP: [usize; 64] = [
    0,  0,  1,  1,  5,  5,  4,  4,
    2,  2,  2,  2,  6,  6,  6,  6,
    2,  2,  2,  2,  6,  6,  6,  6,
    3,  3,  3,  3,  7,  7,  7,  7,
    3,  3,  3,  3,  7,  7,  7,  7,
    3,  3,  3,  3,  7,  7,  7,  7,
    3,  3,  3,  3,  7,  7,  7,  7,
    3,  3,  3,  3,  7,  7,  7,  7,
];

#[repr(C)]
#[rustfmt::skip]
pub struct NNUEParams {
    pub feature_weights: Align64<[i16; INPUT * L1 * NUM_INPUT_BUCKETS]>,
    pub feature_bias:    Align64<[i16; L1]>,
    pub output_weights:  [[Align64<[i16; L1]>; 2]; NUM_OUTPUT_BUCKETS],
    pub output_bias:    Align64<[i16; NUM_OUTPUT_BUCKETS]>
}

pub static NNUE_EMBEDDED: NNUEParams =
    unsafe { mem::transmute(*include_bytes!("../data/big.nnue")) };
