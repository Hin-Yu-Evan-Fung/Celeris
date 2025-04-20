use sophos::board::fen::*;
use sophos::board::*;
use sophos::core::*;
use sophos::movegen::move_gen::generate_move;
use sophos::movegen::*;
use sophos::utils::PRNG;

fn perft(board: &mut Board, depth: usize) -> usize {
    let mut move_list = MoveList::new();

    generate_move::<LegalGen>(board, &mut move_list);

    if depth == 1 {
        return move_list.len();
    }

    let mut nodes = 0;

    for move_ in move_list.iter() {
        board.make_move(*move_);
        nodes += perft(board, depth - 1);
        board.undo_move(*move_);
    }

    nodes
}

fn perft_test(board: &mut Board, depth: usize) {
    let mut move_list = MoveList::new();

    generate_move::<LegalGen>(board, &mut move_list);

    let mut total_nodes = 0;

    for move_ in move_list.iter() {
        board.make_move(*move_);
        let nodes = perft(board, depth - 1);
        total_nodes += nodes;
        board.undo_move(*move_);

        println!("{move_}: {nodes:?}");
    }

    println!("Total nodes: {total_nodes}");
}

fn main() {
    let mut board = Board::from_fen(TRICKY_FEN).unwrap();
    board.make_move(Move::new(Square::E1, Square::D1, MoveFlag::QuietMove));
    println!("{}", board);
    perft_test(&mut board, 3);
}
