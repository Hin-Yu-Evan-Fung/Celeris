use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

use chess::board::Board;

use crate::{
    Eval, INFINITY,
    search::{PVLine, SearchStack},
    types::TT,
};

#[derive(Debug, Default, Clone)]
pub struct SearchWorker {
    board: Board,

    thread_id: usize,
    stack: SearchStack,

    nodes: u64,
    seldepth: usize,
    ply: usize,
    ply_from_null: usize,

    pv: PVLine,
    completed_depth: usize,
    eval: Eval,
}

impl SearchWorker {
    pub fn new(thread_id: usize) -> Self {
        Self {
            thread_id,
            eval: -INFINITY,
            ..Default::default()
        }
    }

    pub fn thread_id(&self) -> usize {
        self.thread_id
    }

    pub fn reset(&mut self) {
        self.stack.clear();
        self.nodes = 0;
        self.seldepth = 0;
        self.ply = 0;
        self.ply_from_null = 0;
        self.pv.clear();
        self.completed_depth = 0;
        self.eval = -INFINITY;
    }

    pub fn search(&mut self, stop: Arc<AtomicBool>, tt: &TT) {}
}
