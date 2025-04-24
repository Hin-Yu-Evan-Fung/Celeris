use std::sync::{Arc, atomic::AtomicBool, mpsc::Receiver};

use crate::uci::UCICommand;
use chess::{
    board::Board,
    utils::{perft_bench, perft_test},
};

pub struct Engine {
    board: Board,
    stop: Arc<AtomicBool>,
}

impl Engine {
    pub fn run(rx: Receiver<UCICommand>, stop: Arc<AtomicBool>) {
        let mut engine = Self {
            board: Board::default(),
            stop,
        };

        for command in rx {
            engine.handle_command(command);
        }
    }

    fn handle_command(&mut self, command: UCICommand) {
        match command {
            UCICommand::UciNewGame => self.board = Board::default(),
            UCICommand::Position(board) => self.board = board,
            UCICommand::Perft(depth) => perft_test(&mut self.board, depth),
            UCICommand::Bench => println!("Bench successful? {}", perft_bench()),
            UCICommand::Print => println!("{}", self.board),
            UCICommand::Eval => {}
            _ => {}
        }
    }
}
