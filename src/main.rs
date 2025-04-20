use sophos::board::*;
use sophos::core::*;
use sophos::utils::*;

fn main() {
    perft_bench();
    // let mut board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
    // println!("{}", board);
    // board.make_move(Move::new(Square::A5, Square::A4, MoveFlag::QuietMove));
    // board.make_move(Move::new(Square::H4, Square::G3, MoveFlag::QuietMove));
    // board.make_move(Move::new(Square::B4, Square::B2, MoveFlag::QuietMove));
    // board.make_move(Move::new(Square::G3, Square::F2, MoveFlag::QuietMove));
    // board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
    // perft_test(&mut board, 2);
}
