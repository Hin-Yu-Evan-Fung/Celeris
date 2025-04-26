use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
    mpsc::Receiver,
};

use chess::{
    board::Board,
    utils::{perft_bench, perft_test},
};

pub use super::UCICommand;

mod constants {
    pub const NAME: &str = "Celeris";
    pub const VERSION: &str = "0.0.1";
    pub const AUTHORS: &str = "TheGogy, 0mn1verze";
}

#[derive(Default)]
pub struct Engine {
    is_debug: bool,
    board: Board,
    stop: Arc<AtomicBool>,
}

impl Engine {
    pub fn run_uci(&mut self, rx: Receiver<UCICommand>) {
        for command in rx {
            self.handle_command(command);
        }
    }

    fn handle_command(&mut self, command: UCICommand) {
        match command {
            UCICommand::Uci => self.introduce(),
            UCICommand::Debug(is_debug) => self.set_debug(is_debug),
            UCICommand::IsReady => println!("readyok"),
            UCICommand::UciNewGame => self.new_game(),
            UCICommand::SetOption(name, value) => self.set_option(name, value),
            UCICommand::Position(board) => self.set_position(board),
            UCICommand::Perft(depth) => self.perft(depth),
            UCICommand::Bench => self.bench(),
            UCICommand::Print => self.print_board(),
            UCICommand::Eval => {}
            UCICommand::Stop => self.stop(),
            _ => unreachable!(), // UCICommand::Quit is already handled by the UCI struct
        }
    }

    fn introduce(&self) {
        println!("id name {} {}", constants::NAME, constants::VERSION);
        println!("id author {}", constants::AUTHORS);
        println!("uciok");
    }

    pub fn set_debug(&mut self, is_debug: bool) {
        self.is_debug = is_debug;
    }

    pub fn new_game(&mut self) {
        self.board = Board::default();
    }

    pub fn set_option(&mut self, name: String, value: String) {}

    pub fn set_position(&mut self, board: Board) {
        self.board = board;
    }

    pub fn bench(&self) {
        perft_bench();
    }

    pub fn perft(&mut self, depth: usize) {
        perft_test(&mut self.board, depth);
    }

    pub fn print_board(&self) {
        println!("{}", self.board)
    }

    pub fn stop(&self) {
        self.stop.store(true, Ordering::Relaxed);
    }
}
