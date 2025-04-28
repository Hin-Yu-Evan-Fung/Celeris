// use engine::{Engine, UCI};
use chess::board::{Board, TRICKY_FEN};
use engine::{Engine, TimeControl};

fn main() {
    // UCI::init();

    let mut engine = Engine::new();

    engine.set_position(Board::from_fen(TRICKY_FEN).unwrap());

    engine.go(TimeControl::FixedDepth(7));

    // let mut move_list = MoveList::new();

    // board.generate_moves::<CaptureGen>(&mut move_list);

    // board.generate_moves::<QuietGen>(&mut move_list);

    // for move_ in move_list.iter() {
    //     println!("{move_}");
    // }

    // println!("{}", move_list.iter().count());
}
