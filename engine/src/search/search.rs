use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicU64},
};

use chess::{Move, MoveFlag, board::Board};

use crate::{
    INFINITY,
    engine::MAX_DEPTH,
    eval::{self, EVAL_ZERO, Eval, evaluate},
    movepick::MovePicker,
    search::{PVLine, SearchStack},
    types::TT,
};

use super::{Clock, NodeType, NodeTypeTrait, PVNode, RootNode};

#[derive(Debug, Clone)]
pub struct SearchWorker {
    pub clock: Clock,
    board: Board,

    thread_id: usize,
    stack: SearchStack,

    nodes: u64,
    depth: usize,
    seldepth: usize,
    ply: u16,

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
        self.pv = PVLine::default();
        self.completed_depth = 0;
        self.eval = -INFINITY;
        self.stop = false;
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
        self.depth = 0;

        while self.should_start_iteration() {
            self.search_position(tt);

            if self.stop {
                break;
            }

            self.depth += 1;
        }
    }

    fn search_position(&mut self, tt: &TT) {
        let alpha = -INFINITY;
        let beta = INFINITY;

        let mut pv = PVLine::default();

        let eval = self.negamax::<RootNode>(&mut pv, alpha, beta, self.depth + 1);

        self.pv = pv;

        // println!("{}", self.board);

        // println!("Static eval: {}", evaluate(&self.board));

        println!(
            "info depth {} seldepth {} score {} time {} nodes {} {}",
            self.depth + 1,
            self.seldepth,
            eval,
            self.clock.elapsed().as_millis(),
            self.nodes,
            self.pv
        )
    }

    fn negamax<NT: NodeTypeTrait>(
        &mut self,
        pv: &mut PVLine,
        mut alpha: Eval,
        mut beta: Eval,
        mut depth: usize,
    ) -> Eval {
        if self.should_stop_search() {
            self.stop = true;
            return EVAL_ZERO;
        }

        let in_check = self.board.in_check();

        if depth <= 0 {
            return self.quiescence::<PVNode>(pv, alpha, beta);
        }

        pv.clear();

        let mut child_pv = PVLine::default();

        // depth = depth.max(0);

        // if NT::node_type() != NodeType::Root {
        //     if self.board.is_draw(self.board.half_moves()) {
        //         return EVAL_ZERO;
        //     }
        // }

        let mut best_value = -INFINITY;
        let mut move_count = 0;

        let mut move_picker = MovePicker::<false>::new();

        while let Some(move_) = move_picker.next(&self.board) {
            self.board.make_move(move_);

            self.ply += 1;
            self.nodes += 1;

            let value = -self.negamax::<PVNode>(&mut child_pv, -beta, -alpha, depth - 1);

            self.board.undo_move(move_);

            self.ply -= 1;

            move_count += 1;

            if self.stop {
                return EVAL_ZERO;
            }

            if NT::node_type() == NodeType::Root {
                println!("{move_}: {value}");
            }

            if value > best_value {
                best_value = value;

                if value > alpha {
                    pv.update_line(move_, &child_pv);

                    alpha = value;
                }
            }

            if value >= beta {
                return best_value;
            }
        }

        // if move_count == 0 {
        //     best_value = if in_check {
        //         Eval::mated_in(self.ply)
        //     } else {
        //         EVAL_ZERO
        //     };
        // }

        best_value
    }

    fn quiescence<NT: NodeTypeTrait>(
        &mut self,
        pv: &mut PVLine,
        mut alpha: Eval,
        mut beta: Eval,
    ) -> Eval {
        return evaluate(&self.board);
        pv.clear();

        if self.should_stop_search() {
            self.stop = true;
            return EVAL_ZERO;
        }

        let in_check = self.board.in_check();

        // if self.ply >= MAX_DEPTH as u16 {
        //     return if in_check {
        //         EVAL_ZERO
        //     } else {
        //         evaluate(&self.board)
        //     };
        // }

        // if self.board.is_draw(self.board.half_moves()) {
        //     return EVAL_ZERO;
        // }

        let mut child_pv = PVLine::default();

        let eval = evaluate(&self.board);

        let mut best_value = eval;

        if eval >= beta {
            return eval;
        }

        alpha = alpha.max(eval);

        let mut move_picker = MovePicker::<true>::new();

        while let Some(move_) = move_picker.next(&self.board) {
            self.board.make_move(move_);
            self.ply += 1;
            self.nodes += 1;

            let value = -self.quiescence::<PVNode>(&mut child_pv, -beta, -alpha);

            self.board.undo_move(move_);
            self.ply -= 1;

            if self.stop {
                return EVAL_ZERO;
            }

            if value >= beta {
                return value;
            }

            if value > best_value {
                best_value = value;
            }

            if value > alpha {
                // pv.load(move_, &child_pv);
                alpha = value;
            }
        }

        // if in_check && best_value == -INFINITY {
        //     return Eval::mated_in(self.ply);
        // }

        best_value
    }
}
