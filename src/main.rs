use sophos::core::board::fen::*;
use sophos::core::*;
use sophos::core::{Board, BoardState};
use sophos::movegen::{init_all_tables, slider_attack};

fn main() {
    use std::time::Instant;
    // Time the initialization of the move generation tables
    let start = Instant::now();
    // Initialize the move generation tables
    init_all_tables();
    let duration = start.elapsed();
    println!(
        "Time taken to initialize the move generation tables: {:?}",
        duration
    );

    let mut board = Board::new();

    board
        .set("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 2")
        .unwrap();

    println!("{}", board.fen());
}
