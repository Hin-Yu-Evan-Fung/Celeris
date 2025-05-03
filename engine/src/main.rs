use chess::{Board, Move, board::TRICKY_FEN};
use engine::{MovePicker, SearchStats, UCI, run_bench};
use std::env::args;

fn main() {
    match args().nth(1).as_deref() {
        Some("bench") => run_bench(),
        _ => UCI::init(),
    }
}
