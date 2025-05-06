//! # Engine Interface Module
//!
//! This module provides the primary interfaces for interacting with the Celeris chess engine.
//!
//! - [`Engine`]: A struct providing programmatic control over the engine (e.g., for testing or embedding).
//!   It runs the core engine logic in a separate thread and communicates via commands.
//! - [`UCI`]: A struct that handles the Universal Chess Interface (UCI) protocol, allowing interaction
//!   with standard chess GUIs via standard input/output.

// Import necessary modules from the parent 'engine' module and standard library.
use super::{
    TimeControl,
    command::EngineOption,
    engine::{Command, EngineController},
};
use chess::{Board, board::movegen::init_magic_tables};
use std::{
    io::BufRead,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Sender},
    },
    thread::{self, JoinHandle},
};

/// Represents the main engine interface for external interaction (e.g., GUI or tests).
///
/// This struct allows you to send commands to the chess engine, which runs in a
/// background thread. It's useful for integrating the engine into other applications
/// or for writing automated tests.
///
/// # Example
///
/// ```no_run
/// use celeris::{Engine, TimeControl};
/// use chess::Board;
/// use std::time::Duration;
///
/// // Create a new engine instance (starts the background thread)
/// let engine = Engine::new();
///
/// // Set the position
/// engine.position(Board::default());
///
/// // Start searching for the best move with a 5-second time limit
/// engine.go(TimeControl::FixedTime(5000));
///
/// // Give the engine some time to search (in a real app, you'd wait for UCI output)
/// std::thread::sleep(Duration::from_secs(6));
///
/// // The engine will print the best move to stdout via the UCI protocol.
/// // The Engine instance will automatically stop the background thread when dropped.
/// ```
/// This struct provides methods to send commands to the engine controller running in a separate thread.
/// It manages the communication channel and the lifecycle of the engine thread.
pub struct Engine {
    command_tx: Sender<Command>,
    stop: Arc<AtomicBool>,
    handle: Option<JoinHandle<()>>,
}

impl Engine {
    /// Creates a new `Engine` instance.
    ///
    /// This spawns a new thread where the [`EngineController`] runs, listening for commands.
    /// The engine starts with the default board position and settings.
    ///
    /// The background thread is automatically managed and will be stopped when the
    /// `Engine` instance is dropped.
    /// This spawns a new thread where the `EngineController` runs, listening for commands.
    pub fn new() -> Self {
        // initialise magic tables
        init_magic_tables();
        // Create a channel for sending commands to the engine thread.
        let (tx, rx) = mpsc::channel();
        // Create a shared atomic boolean to signal the engine thread to stop.
        let stop = Arc::new(AtomicBool::new(false));
        // Create the engine controller which will run in the background thread.
        let mut engine = EngineController::new(stop.clone());

        let handle = Some(thread::spawn(move || engine.run(rx)));

        Self {
            command_tx: tx,
            stop,
            handle,
        }
    }

    /// Sets the debug mode for the engine.
    ///
    /// When debug mode is on, the engine may print additional information
    /// about its internal state or search process (via UCI `info string` commands).
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// let engine = Engine::new();
    /// engine.set_debug(true); // Enable debug output
    /// engine.set_debug(false); // Disable debug output
    /// ```
    pub fn set_debug(&self, debug: bool) {
        self.command_tx.send(Command::Debug(debug)).unwrap();
    }

    /// Sends a command to clear the transposition table.
    ///
    /// This removes all stored search information. Useful when starting a new
    /// analysis or to ensure a fresh search.
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// let engine = Engine::new();
    /// engine.clear_hash();
    /// ```
    pub fn clear_hash(&self) {
        self.command_tx
            .send(Command::SetOption(EngineOption::ClearHash()))
            .unwrap();
    }

    /// Sends a command to resize the transposition table (hash).
    ///
    /// `mb`: The desired size in megabytes.
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// let engine = Engine::new();
    /// // Set hash size to 128 MB
    /// engine.resize_hash(128);
    /// ```
    pub fn resize_hash(&self, mb: usize) {
        self.command_tx
            .send(Command::SetOption(EngineOption::ResizeHash(mb)))
            .unwrap();
    }

    /// Sends the UCI `isready` command.
    ///
    /// The engine controller will respond by printing `readyok` to standard output
    /// when it has finished processing any pending commands and is ready for the next one.
    /// This is often used for synchronization.
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// let engine = Engine::new();
    /// engine.is_ready();
    /// // In a real application, you would typically read stdout here
    /// // and wait for the "readyok" line.
    /// ```
    pub fn is_ready(&self) {
        self.command_tx.send(Command::IsReady).unwrap();
    }

    /// Sends the UCI `go` command to start searching for the best move.
    ///
    /// `time_control`: Specifies the time constraints for the search.
    /// See the [`TimeControl`] enum for different modes (infinite, fixed depth, fixed time, etc.).
    ///
    /// The engine will start searching and output information (like depth, score, nodes)
    /// and eventually the `bestmove` via the UCI protocol to standard output.
    ///
    /// ```no_run
    /// # use celeris::{Engine, TimeControl};
    /// # use std::time::Duration;
    /// let engine = Engine::new();
    /// engine.position(Default::default()); // Set a position first
    ///
    /// // Search for 10 seconds
    /// engine.go(TimeControl::FixedTime(10000));
    ///
    /// // Allow time for search (replace with proper UCI output handling)
    /// std::thread::sleep(Duration::from_secs(11));
    /// ```
    pub fn go(&self, time_control: TimeControl) {
        self.command_tx.send(Command::Go(time_control)).unwrap();
    }

    /// Sends the UCI `position` command to set the current board position.
    ///
    /// `board`: The `chess::Board` representing the position.
    ///
    /// This updates the engine's internal board state.
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// # use chess::{Board, Square, Move};
    /// let engine = Engine::new();
    ///
    /// // Set the starting position
    /// engine.position(Board::default());
    ///
    /// // Set a position after 1. e4 e5
    /// let mut board = Board::default();
    /// board.make_move(Move::new(Square::E2, Square::E4, None));
    /// board.make_move(Move::new(Square::E7, Square::E5, None));
    /// engine.position(board);
    /// ```
    pub fn position(&self, board: Board) {
        self.command_tx.send(Command::Position(board)).unwrap();
    }

    /// Sends the UCI `ucinewgame` command.
    ///
    /// This signals to the engine that the next `position` and `go` commands
    /// belong to a new game. The engine might reset internal state like history tables,
    /// but typically keeps the transposition table unless explicitly cleared.
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// let engine = Engine::new();
    /// // ... play a game ...
    /// engine.new_game();
    /// // ... set up position for the next game ...
    /// ```
    pub fn new_game(&self) {
        self.command_tx.send(Command::NewGame).unwrap();
    }

    /// Sends a custom `perft` command to run a performance test (move generation count).
    ///
    /// `depth`: The depth for the perft calculation.
    ///
    /// The engine will calculate and print the number of leaf nodes at the specified depth.
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// let engine = Engine::new();
    /// engine.position(Default::default()); // Set position first
    /// engine.perft(5); // Calculate perft for depth 5
    /// ```
    pub fn perft(&self, depth: usize) {
        self.command_tx.send(Command::Perft(depth)).unwrap();
    }

    /// Sends a custom `print` command to display the current board state to the console.
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// # use chess::Board;
    /// let engine = Engine::new();
    /// engine.position(Board::default());
    /// engine.print_board(); // Prints the starting position board
    /// ```
    pub fn print_board(&self) {
        self.command_tx.send(Command::Print).unwrap();
    }

    /// Sends a custom `eval` command to evaluate the current board position and print the score.
    ///
    /// ```no_run
    /// # use celeris::Engine;
    /// # use chess::Board;
    /// let engine = Engine::new();
    /// engine.position(Board::default());
    /// engine.eval(); // Prints the static evaluation of the starting position
    /// ```
    pub fn eval(&self) {
        self.command_tx.send(Command::Eval).unwrap();
    }
}

impl Drop for Engine {
    fn drop(&mut self) {
        // Signal the engine thread to stop.
        self.stop.store(true, std::sync::atomic::Ordering::Relaxed);
        // Wait for the engine thread to finish.
        if let Some(handle) = self.handle.take() {
            handle.join().unwrap();
        }
    }
}

/// Represents the UCI (Universal Chess Interface) handler.
///
/// This struct is intended to be the main entry point for running the engine
/// in UCI mode, suitable for interaction with graphical user interfaces (GUIs) like
/// Arena, Cute Chess, etc.
///
/// It takes control of the main thread, reads UCI commands from standard input,
/// parses them, and interacts with the [`EngineController`] running in a background thread.
/// This struct manages the main loop for interacting with a UCI-compliant GUI.
/// It reads commands from standard input, parses them, and sends them to the engine controller.
pub struct UCI {
    command_tx: Sender<Command>,
    stop: Arc<AtomicBool>,
}

impl UCI {
    /// Initializes the UCI interface and starts the engine.
    ///
    /// This function should typically be called from your `main` function when
    /// the engine is run without specific command-line arguments (like "bench").
    ///
    /// It performs the following steps:
    /// 1. Creates the communication channel for engine commands.
    /// 2. Spawns the background thread running the [`EngineController`].
    /// 3. Creates the `UCI` handler instance.
    /// 4. Enters the main UCI command loop (`run`), reading from stdin.
    /// This function takes control of the main thread, running the UCI command loop.
    pub fn init() {
        // initialise magic tables
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

    /// Runs the main UCI command loop.
    /// Reads lines from standard input, parses them as `Command`s, and handles them.
    pub fn run(&self) {
        let stdin = std::io::stdin().lock();
        for line in stdin.lines().map(Result::unwrap) {
            match line.parse::<Command>() {
                Ok(command) => self.handle_command(command),
                Err(e) => println!("{}", e),
            }
        }
    }

    /// Handles a parsed `Command`.
    /// Sends most commands to the engine thread, but handles `Quit` and `Stop` locally.
    fn handle_command(&self, command: Command) {
        match command {
            Command::Quit => self.quit(),
            Command::Stop => self.stop(),
            // Forward other commands to the engine controller thread.
            command => self.command_tx.send(command).unwrap(),
        }
    }

    /// Signals the engine thread to stop its current task (e.g., searching).
    fn stop(&self) {
        self.stop.store(true, Ordering::Relaxed);
    }

    /// Handles the "quit" command. Stops the engine and exits the process.
    fn quit(&self) {
        self.stop();
        std::process::exit(0);
    }
}
