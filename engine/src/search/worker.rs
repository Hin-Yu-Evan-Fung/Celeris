use crate::{
    Eval, INFINITY,
    search::{PVLine, SearchStack},
};

#[derive(Debug, Default, Clone)]
pub struct SearchWorker {
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
}
