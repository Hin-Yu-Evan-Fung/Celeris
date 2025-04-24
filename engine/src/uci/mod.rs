use std::{
    io::BufRead,
    sync::{
        Arc,
        atomic::AtomicBool,
        mpsc::{self, Receiver, Sender},
    },
    thread,
};

use chess::utils::{perft_bench, perft_test};

struct TimeControl {}

mod command;
use super::engine::Engine;
pub use command::UCICommand;

pub struct UCI {
    stop: Arc<AtomicBool>,
    command_tx: Sender<UCICommand>,
}

impl UCI {
    pub fn init() -> Self {
        let (tx, rx) = mpsc::channel();
        let stop = Arc::new(AtomicBool::new(false));
        let engine_stop = stop.clone();

        thread::spawn(move || {
            Engine::run(rx, engine_stop);
        });

        Self {
            stop,
            command_tx: tx,
        }
    }

    pub fn run(&self) {
        let stdin = std::io::stdin().lock();
        for line in stdin.lines().map(Result::unwrap) {
            match line.parse::<UCICommand>() {
                Ok(command) => self.handle_command(command),
                Err(e) => println!("{}", e),
            }
        }
    }

    fn handle_command(&self, command: UCICommand) {
        match command {
            UCICommand::Uci => {}
            UCICommand::IsReady => println!("readyok"),
            UCICommand::Quit => self.quit(),
            UCICommand::Stop => self.stop(),
            command => self.command_tx.send(command).unwrap(),
        }
    }

    fn stop(&self) {
        self.stop.store(true, std::sync::atomic::Ordering::Relaxed);
    }

    fn quit(&self) {
        self.stop();
        std::process::exit(0);
    }
}
