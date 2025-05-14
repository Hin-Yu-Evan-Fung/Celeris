use super::Command;
use super::engine::EngineController;
use chess::board::movegen::init_magic_tables;
use std::{
    io::BufRead,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Sender},
    },
    thread,
};

pub struct UCI {
    command_tx: Sender<Command>,
    stop: Arc<AtomicBool>,
}

impl UCI {
    pub fn init() {
        init_magic_tables();

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
