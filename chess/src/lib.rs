//! # Celeris Chess Engine
//!
//! A Rust chess engine focusing on modern techniques and high performance.
pub mod board;
pub mod core;
pub mod utils;

pub use board::{Board, BoardState};
pub use core::*;
