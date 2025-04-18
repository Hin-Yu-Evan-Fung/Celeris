use sophos::board::fen::*;
use sophos::board::*;
use sophos::core::*;
use sophos::movegen::lookup::*;

fn main() {
    let fen = "2r1k3/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b q - 0 1";

    let mut board = Board::from_fen(fen).unwrap();

    println!("{}", board);

    board.make_move(Move::new(Square::E8, Square::C8, MoveFlag::QueenCastle));

    println!("{}", board);

    board.undo_move(Move::new(Square::E8, Square::C8, MoveFlag::QueenCastle));

    println!("{}", board);

    println!("{:?}", board.castling_mask.castling);
}
