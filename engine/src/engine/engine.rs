use std::sync::{Arc, atomic::AtomicBool, mpsc::Receiver};

use chess::{
    board::Board,
    utils::{perft_bench, perft_test},
};

pub use super::EngineCommand;

mod constants {
    pub const NAME: &str = "Celeris";
    pub const VERSION: &str = "0.0.1";
    pub const AUTHORS: &str = "TheGogy, 0mn1verze";
}

pub struct Engine {
    board: Board,
    stop: Arc<AtomicBool>,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            board: Board::default(),
            stop: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn run_uci(rx: Receiver<EngineCommand>, stop: Arc<AtomicBool>) {
        let mut engine = Self {
            board: Board::default(),
            stop,
        };

        for command in rx {
            engine.handle_command(command);
        }
    }

    fn handle_command(&mut self, command: EngineCommand) {
        match command {
            EngineCommand::Uci => self.introduce(),
            EngineCommand::UciNewGame => self.new_game(),
            EngineCommand::Position(board) => self.set_position(board),
            EngineCommand::Perft(depth) => self.perft(depth),
            EngineCommand::Bench => self.bench(),
            EngineCommand::Print => self.print_board(),
            EngineCommand::Eval => {}
            _ => {}
        }
    }

    fn introduce(&self) {
        println!("id name {} {}", constants::NAME, constants::VERSION);
        println!("id author {}", constants::AUTHORS);
        println!("uciok");
    }

    pub fn new_game(&mut self) {
        self.board = Board::default();
    }

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
}
