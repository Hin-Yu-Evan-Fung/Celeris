use super::engine::{Engine, EngineCommand};
use std::{
    io::BufRead,
    sync::{Arc, atomic::AtomicBool, mpsc, mpsc::Sender},
    thread,
};

pub struct UCI {
    stop: Arc<AtomicBool>,
    command_tx: Sender<EngineCommand>,
}

impl UCI {
    pub fn init() -> Self {
        let (tx, rx) = mpsc::channel();
        let stop = Arc::new(AtomicBool::new(false));
        let engine_stop = stop.clone();

        thread::spawn(move || {
            Engine::run_uci(rx, engine_stop);
        });

        Self {
            stop,
            command_tx: tx,
        }
    }

    pub fn run(&self) {
        let stdin = std::io::stdin().lock();
        for line in stdin.lines().map(Result::unwrap) {
            match line.parse::<EngineCommand>() {
                Ok(command) => self.handle_command(command),
                Err(e) => println!("{}", e),
            }
        }
    }

    fn handle_command(&self, command: EngineCommand) {
        match command {
            EngineCommand::Uci => {}
            EngineCommand::IsReady => println!("readyok"),
            EngineCommand::Quit => self.quit(),
            EngineCommand::Stop => self.stop(),
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
