use engine::{UCI, run_bench};
use std::env::args;

fn main() {
    match args().nth(1).as_deref() {
        Some("bench") => run_bench(),
        _ => UCI::init(),
    }
}
