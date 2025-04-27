use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    time::Duration,
};

use chess::{Move, board::Board};

use crate::{
    Eval, INFINITY,
    engine::MAX_DEPTH,
    search::{PVLine, SearchStack},
    types::TT,
};

use super::Clock;

#[derive(Debug, Clone)]
pub struct SearchWorker {
    pub clock: Clock,
    board: Board,

    thread_id: usize,
    stack: SearchStack,

    nodes: u64,
    depth: usize,
    seldepth: usize,
    ply: usize,
    ply_from_null: usize,

    pv: PVLine,
    completed_depth: usize,
    eval: Eval,

    stop: bool,
}

impl SearchWorker {
    pub fn new(thread_id: usize, stop: Arc<AtomicBool>, nodes: Arc<AtomicU64>) -> Self {
        Self {
            clock: Clock::default(stop, nodes),
            thread_id,
            eval: -INFINITY,
            board: Board::default(),
            stack: SearchStack::default(),
            nodes: 0,
            seldepth: 0,
            depth: 0,
            ply: 0,
            ply_from_null: 0,
            pv: PVLine::default(),
            completed_depth: 0,
            stop: false,
        }
    }

    pub fn thread_id(&self) -> usize {
        self.thread_id
    }

    pub fn reset(&mut self) {
        self.stack.clear();
        self.clock.last_nodes = 0;
        self.nodes = 0;
        self.seldepth = 0;
        self.ply = 0;
        self.ply_from_null = 0;
        self.pv.clear();
        self.completed_depth = 0;
        self.eval = -INFINITY;
    }

    pub fn setup(&mut self, board: Board) {
        self.board = board;
    }

    pub const fn best_move(&self) -> Move {
        self.pv.moves[0]
    }

    // Search control
    pub fn should_start_iteration(&mut self) -> bool {
        self.depth < MAX_DEPTH
            && self
                .clock
                .start_search(self.depth + 1, self.nodes, self.best_move())
    }

    pub fn should_stop_search(&mut self) -> bool {
        self.stop || !self.clock.continue_search(self.nodes)
    }

    pub fn start_search(&mut self, tt: &TT) {
        self.iterative_deepening(tt);
    }

    fn iterative_deepening(&mut self, tt: &TT) {
        self.depth = 1;

        while self.should_start_iteration() {
            println!("Depth: {}", self.depth);

            self.search_position(tt);

            if self.stop {
                break;
            }

            self.depth += 1;
        }
    }

    fn search_position(&mut self, tt: &TT) {
        let mut alpha = -INFINITY;
        let mut beta = INFINITY;

        let eval = self.negamax(alpha, beta, self.depth);
    }

    fn negamax(&mut self, mut alpha: Eval, mut beta: Eval, depth: usize) -> Eval {
        println!("{}", self.board);

        Eval(0)
    }
}
