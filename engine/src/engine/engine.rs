use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
    mpsc::Receiver,
};

use chess::{
    board::Board,
    utils::{perft_bench, perft_test},
};

use crate::{thread::ThreadPool, types::TT};

use super::command::UCIOption;
pub use super::{TimeControl, UCICommand};

pub mod constants {

    use crate::eval::Eval;
    use chess::board::MAX_MOVES;

    pub const NAME: &str = "Celeris";
    pub const VERSION: &str = "0.0.1";
    pub const AUTHORS: &str = "0mn1verze, TheGogy";

    pub const THREADS: usize = 1;
    pub const DEBUG: bool = true;
    pub const TT_SIZE: usize = 64;

    pub const MAX_DEPTH: usize = MAX_MOVES;
    pub const INFINITY: Eval = Eval(32001);
    pub const MATE: Eval = Eval(32000);
    pub const LONGEST_MATE: Eval = Eval(MAX_DEPTH as i16);
    pub const MATE_BOUND: Eval = MATE.sub(LONGEST_MATE);
}

pub struct Engine {
    is_debug: bool,
    board: Board,
    tt: TT,
    thread_pool: ThreadPool,
    stop: Arc<AtomicBool>,
}

impl Engine {
    pub fn new() -> Self {
        let stop = Arc::new(AtomicBool::new(false));
        let tt = TT::default();
        let mut thread_pool = ThreadPool::new(Arc::clone(&stop));

        thread_pool.resize(constants::THREADS);

        Self {
            is_debug: constants::DEBUG,
            board: Board::default(),
            tt,
            thread_pool,
            stop,
        }
    }

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
            UCICommand::SetOption(option) => self.set_option(option),
            UCICommand::Position(board) => self.set_position(board),
            UCICommand::Go(time_control) => self.go(time_control),
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

    pub fn resize_hash(&mut self, size_mb: usize) {
        if self.is_debug {
            println!("info string Attempting to resize hash to {} MB...", size_mb);
        }

        self.tt.resize(size_mb);

        if self.is_debug {
            println!("info string Hash resized to {} MB.", size_mb);
            println!("info string Hash entries: {}.", self.tt.size());
        }
    }

    pub fn clear_hash(&mut self) {
        if self.is_debug {
            println!("info string Attempting to clear hash table...");
        }

        self.thread_pool.clear_hash_table(&self.tt);

        if self.is_debug {
            println!("info string Hash cleared.");
        }
    }

    pub fn resize_threads(&mut self, threads: usize) {
        if self.is_debug {
            println!(
                "info string Attempting to resize the number of threads to {} ",
                threads
            );
        }

        self.thread_pool.resize(threads);

        if self.is_debug {
            println!("info string Threads resized to {} ", threads);
        }
    }

    pub fn set_option(&mut self, option: UCIOption) {
        match option {
            UCIOption::ClearHash() => self.clear_hash(),
            UCIOption::ResizeHash(size_mb) => self.resize_hash(size_mb),
            UCIOption::ResizeThreads(threads) => self.resize_threads(threads),
        }
    }

    pub fn set_position(&mut self, board: Board) {
        self.board = board;
    }

    pub fn go(&mut self, time_control: TimeControl) {
        self.thread_pool
            .start_search(time_control, &self.tt, &self.board);
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
