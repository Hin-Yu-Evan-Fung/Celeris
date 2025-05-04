use chess::{Board, Move, board::TRICKY_FEN};
use engine::{MovePicker, SearchStats, UCI, run_bench};
use std::env::args;

fn main() {
    // let mut board = Board::from_fen(TRICKY_FEN).unwrap();

    // let mut move_picker = MovePicker::<false>::new(&board, Move::NONE, [Move::NONE, Move::NONE]);
    // let mut search_stats = SearchStats::default();

    // let mut count = 0;

    // while let Some(move_) = move_picker.next(&board, &search_stats) {
    //     count += 1;
    //     println!("{move_}");
    // }

    // println!("count: {count}");

    match args().nth(1).as_deref() {
        Some("bench") => run_bench(),
        _ => UCI::init(),
    }
}
