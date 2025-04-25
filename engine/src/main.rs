// use chess::utils::perft_bench;
use chess::board::Board;
use engine::Engine;
use engine::uci::UCI;

fn main() {
    // perft_bench();
    // UCI::init().run();

    let mut engine = Engine::new();

    engine.set_position(Board::default());

    engine.perft(6);
}
