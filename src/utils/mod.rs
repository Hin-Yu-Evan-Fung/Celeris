pub mod magic_gen;
pub(crate) mod misc;
pub(crate) mod prng;

pub use magic_gen::{find_best_magic_seeds, gen_slider_attacks};
pub(crate) use misc::abs_diff;
pub(crate) use prng::PRNG;
