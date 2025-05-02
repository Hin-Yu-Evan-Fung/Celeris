use chess::{Board, board::TRICKY_FEN};
use engine::{MovePicker, SearchStats, UCI, run_bench};
use std::env::args;

fn main() {
    match args().nth(1).as_deref() {
        Some("bench") => run_bench(),
        _ => UCI::init(),
    }

    // let mut board = Board::from_fen(TRICKY_FEN).unwrap();

    // let mut move_picker = MovePicker::<false>::new();
    // let mut search_stats = SearchStats::default();

    // while let Some(move_) = move_picker.next(&board, &mut search_stats) {
    //     println!("{move_}");
    // }
}
