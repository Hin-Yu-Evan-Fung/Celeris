// use chess::utils::perft_bench;
use engine::uci::UCI;

fn main() {
    // perft_bench();

    UCI::init().run();
}
