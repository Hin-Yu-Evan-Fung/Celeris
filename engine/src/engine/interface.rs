use chess::board::Board;

use super::{
    TimeControl,
    command::EngineOption,
    engine::{Command, EngineController},
};
use std::{
    io::BufRead,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Sender},
    },
    thread::{self, JoinHandle},
};

pub struct Engine {
    command_tx: Sender<Command>,
    stop: Arc<AtomicBool>,
    handle: Option<JoinHandle<()>>,
}

impl Engine {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();

        let stop = Arc::new(AtomicBool::new(false));

        let mut engine = EngineController::new(stop.clone());

        let handle = Some(thread::spawn(move || engine.run(rx)));

        Self {
            command_tx: tx,
            stop,
            handle,
        }
    }

    pub fn set_debug(&self, debug: bool) {
        self.command_tx.send(Command::Debug(debug)).unwrap();
    }

    pub fn clear_hash(&self) {
        self.command_tx
            .send(Command::SetOption(EngineOption::ClearHash()))
            .unwrap();
    }

    pub fn resize_hash(&self, mb: usize) {
        self.command_tx
            .send(Command::SetOption(EngineOption::ResizeHash(mb)))
            .unwrap();
    }

    pub fn is_ready(&self) {
        self.command_tx.send(Command::IsReady).unwrap();
    }

    pub fn go(&self, time_control: TimeControl) {
        self.command_tx.send(Command::Go(time_control)).unwrap();
    }

    pub fn position(&self, board: Board) {
        self.command_tx.send(Command::Position(board)).unwrap();
    }

    pub fn new_game(&self) {
        self.command_tx.send(Command::NewGame).unwrap();
    }

    pub fn perft(&self, depth: usize) {
        self.command_tx.send(Command::Perft(depth)).unwrap();
    }

    pub fn print_board(&self) {
        self.command_tx.send(Command::Print).unwrap();
    }

    pub fn eval(&self) {
        self.command_tx.send(Command::Eval).unwrap();
    }
}

impl Drop for Engine {
    fn drop(&mut self) {
        self.stop.store(true, std::sync::atomic::Ordering::Relaxed);
        if let Some(handle) = self.handle.take() {
            handle.join().unwrap();
        }
    }
}

pub struct UCI {
    command_tx: Sender<Command>,
    stop: Arc<AtomicBool>,
}

impl UCI {
    pub fn init() {
        let (tx, rx) = mpsc::channel();

        let stop = Arc::new(AtomicBool::new(false));

        let mut engine = EngineController::new(stop.clone());

        let handle = thread::spawn(move || engine.run(rx));

        let uci = Self {
            command_tx: tx,
            stop,
        };

        uci.run();

        handle.join().unwrap();
    }

    pub fn run(&self) {
        let stdin = std::io::stdin().lock();
        for line in stdin.lines().map(Result::unwrap) {
            match line.parse::<Command>() {
                Ok(command) => self.handle_command(command),
                Err(e) => println!("{}", e),
            }
        }
    }

    fn handle_command(&self, command: Command) {
        match command {
            Command::Quit => self.quit(),
            Command::Stop => self.stop(),
            command => self.command_tx.send(command).unwrap(),
        }
    }

    fn stop(&self) {
        self.stop.store(true, Ordering::Relaxed);
    }

    fn quit(&self) {
        self.stop();
        std::process::exit(0);
    }
}
