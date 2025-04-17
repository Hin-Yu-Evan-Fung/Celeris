use sophos::board::fen::*;
use sophos::board::*;
use sophos::core::*;
use sophos::movegen::lookup::*;

fn main() {
    // Setup: Black rook leaves A8
    let fen_before_capture = "rnbqkbnr/1ppppppp/8/p7/P7/8/1PPPPPPP/RNBQKBNR w KQkq - 0 3";
    let mut board = Board::from_fen(fen_before_capture).expect("Test FEN should be valid");
    println!("{}", board);
    board.make_move(Move::new(Square::B2, Square::B4, MoveFlag::QuietMove)); // b4
    board.make_move(Move::new(Square::A5, Square::B4, MoveFlag::Capture)); // b4
    board.make_move(Move::new(Square::A8, Square::A4, MoveFlag::Capture)); // Ra6
    println!("{}", board);
    // Simpler: White Knight captures Black Rook on a8
    let fen_before_capture = "rnbqkbnr/pppppppp/1N6/8/8/8/PPPPPPPP/R1BQKBNR w KQkq - 0 1";
    let mut board_simple = Board::from_fen(fen_before_capture).expect("Test FEN should be valid");
    println!("{}", board_simple);
    board_simple.make_move(Move::new(Square::B6, Square::A8, MoveFlag::Capture)); // Nxa8
    println!("{}", board_simple);
}
