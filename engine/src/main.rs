// use engine::{Engine, UCI};
use chess::Piece;
use chess::board::{Board, LegalGen, MoveList, TRICKY_FEN};
use engine::{EngineController, MovePicker, TimeControl, UCI};

fn main() {
    UCI::init();
}
