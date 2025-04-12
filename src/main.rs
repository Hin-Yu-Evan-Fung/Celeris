use sophos::core::Board;
use sophos::core::fen::*;
use sophos::movegen::init_all_tables;

fn main() {
    // Initialize the move generation tables
    init_all_tables();

    let mut board = Board::new();

    board.set(KILLER_FEN).unwrap();

    println!("{}", board);
}
