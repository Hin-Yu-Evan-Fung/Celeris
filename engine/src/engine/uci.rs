use super::engine::{Engine, UCICommand};
use std::{
    io::BufRead,
    sync::mpsc::{self, Sender},
    thread,
};

pub struct UCI {
    command_tx: Sender<UCICommand>,
}

impl UCI {
    pub fn init() {
        let (tx, rx) = mpsc::channel();

        let mut engine = Engine::new();

        let handle = thread::spawn(move || engine.run_uci(rx));

        let uci = Self { command_tx: tx };

        uci.run();

        handle.join().unwrap();
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
            UCICommand::Quit => self.quit(),
            command => self.command_tx.send(command).unwrap(),
        }
    }

    fn quit(&self) {
        self.command_tx.send(UCICommand::Stop).unwrap();
        std::process::exit(0);
    }
}
