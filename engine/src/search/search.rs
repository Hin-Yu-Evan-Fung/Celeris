use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicU64},
};

use chess::{Move, Piece, board::Board};
use nnue::accummulator::Accumulator;

use crate::{
    History, KillerEntry, MainHistory,
    engine::MAX_DEPTH,
    eval::{Eval, evaluate_nnue},
    movepick::MovePicker,
    search::PVLine,
};

use super::{
    Clock, MIN_DEPTH, NodeType, NonPV, Root, TT,
    tt::{TTBound, TTEntry},
};

#[derive(Debug, Default, Copy, Clone)]
pub(crate) struct SearchStackEntry {
    killers: KillerEntry,
    curr_move: Move,
    excl_move: Move,
    moved: Option<Piece>,
    eval: Eval,
    move_count: u8,
    in_check: bool,
    ply_from_null: u16,
}

#[derive(Debug, Clone, Default)]
pub struct SearchStats {
    pub main_history: MainHistory,
}

const OFFSET: usize = 2;

#[derive(Debug, Clone)]
pub(crate) struct SearchWorker {
    // Search Clock
    pub clock: Clock,
    // Main Board
    board: Board,

    thread_id: usize,

    // Search Stack
    stack: [SearchStackEntry; MAX_DEPTH + OFFSET],

    // Search Info
    nodes: u64,
    depth: usize,
    seldepth: usize,
    ply: u16,
    // Plies from previous null move
    ply_from_null: u16,

    // Move orderin statistics
    stats: SearchStats,

    // Search results
    pv: PVLine,
    eval: Eval,

    // Search thread internal stop flag
    stop: bool,

    // NNUE
    pub nnue: Accumulator,

    // Search stats
    best_first_move: u64,
    best_move_updates: u64,
    tt_move_hit: u64,
    tt_probes: u64,
}

impl SearchWorker {
    pub fn new(thread_id: usize, stop: Arc<AtomicBool>, nodes: Arc<AtomicU64>) -> Self {
        Self {
            clock: Clock::default(stop, nodes),
            thread_id,
            eval: -Eval::INFINITY,
            board: Board::default(),
            stack: [SearchStackEntry::default(); MAX_DEPTH + OFFSET],
            nodes: 0,
            seldepth: 0,
            depth: 0,
            ply: 0,
            ply_from_null: 0,
            pv: PVLine::default(),
            stop: false,
            stats: SearchStats::default(),
            nnue: Accumulator::default(),
            best_first_move: 0,
            best_move_updates: 0,
            tt_move_hit: 0,
            tt_probes: 0,
        }
    }

    pub fn thread_id(&self) -> usize {
        self.thread_id
    }

    pub fn reset(&mut self) {
        self.stats.main_history.clear();
    }

    pub fn prepare_search(&mut self) {
        self.clock.last_nodes = 0;
        self.nodes = 0;
        self.seldepth = 0;
        self.ply = 0;
        self.ply_from_null = 0;
        self.pv = PVLine::default();
        self.eval = -Eval::INFINITY;
        self.stop = false;
        self.tt_move_hit = 0;
        self.tt_probes = 0;
    }

    pub fn setup(&mut self, board: Board) {
        self.board = board;
    }

    pub fn best_move(&self) -> Move {
        self.pv[0]
    }

    // Search control
    pub fn should_start_iteration(&mut self) -> bool {
        self.depth < MAX_DEPTH
            && self
                .clock
                .start_search(self.depth + 1, self.nodes, self.best_move())
    }

    pub fn should_stop_search(&mut self) -> bool {
        // Search up to at least depth 3
        let should_stop =
            self.depth >= MIN_DEPTH && (self.stop || !self.clock.continue_search(self.nodes));
        if should_stop {
            self.stop = true;
        }
        should_stop
    }

    fn ss(&self) -> SearchStackEntry {
        self.stack[self.ply as usize]
    }

    fn ss_mut(&mut self) -> &mut SearchStackEntry {
        &mut self.stack[self.ply as usize]
    }

    fn ss_at(&self, offset: usize) -> SearchStackEntry {
        self.stack[self.ply as usize - offset]
    }

    fn ss_at_mut(&mut self, offset: usize) -> &mut SearchStackEntry {
        &mut self.stack[self.ply as usize - offset]
    }

    pub fn iterative_deepening(&mut self, tt: &TT) {
        self.depth = 0;

        while self.should_start_iteration() {
            self.search_position(tt);

            self.best_move_updates = 0;
            self.best_first_move = 0;

            if self.stop {
                break;
            }

            self.depth += 1;
        }

        println!("bestmove {}", self.pv[0]);
    }

    fn search_position(&mut self, tt: &TT) {
        let alpha = -Eval::INFINITY;
        let beta = Eval::INFINITY;

        let mut pv = PVLine::default();

        let eval = self.negamax::<Root>(tt, &mut pv, alpha, beta, self.depth + 1);

        if self.stop {
            return;
        }

        self.eval = eval;

        self.pv = pv;

        self.print_info(tt);

        println!(
            "First best move percentage: {}",
            self.best_first_move as f64 / self.best_move_updates as f64 * 100.0
        );
        println!(
            "TT move hit percentage: {}",
            self.tt_move_hit as f64 / self.tt_probes as f64 * 100.0
        );
    }

    fn print_info(&self, tt: &TT) {
        let time = self.clock.elapsed().as_millis();

        let nodes_per_second = (self.nodes * 1000) as u128 / time.max(1);

        println!(
            "info depth {} seldepth {} score {} time {} nodes {} nps {} hashfull {} {}",
            self.depth + 1,
            self.seldepth + 1,
            self.eval,
            time,
            self.nodes,
            nodes_per_second,
            tt.hashfull(),
            self.pv
        );
    }

    /// Performs a Negamax search with Alpha-Beta pruning to find the best move and evaluation.
    ///
    /// This is the core recursive function of the chess engine's search algorithm. It explores
    /// the game tree from the current board position (`self.board`) up to a specified `depth`.
    /// It evaluates positions from the perspective of the player whose turn it is (`self.board.stm()`).
    ///
    /// # Algorithm
    ///
    /// Implements the Negamax framework, which simplifies Minimax by always maximizing the score
    /// relative to the current player. The score of the opponent's moves is negated (`-negamax(...)`).
    ///
    /// Alpha-Beta pruning is used to significantly reduce the search space by cutting off branches
    /// that cannot possibly influence the final score within the current `(alpha, beta)` window.
    ///
    /// # Parameters
    ///
    /// * `NT`: A generic type parameter implementing `NodeTypeTrait`. This distinguishes between
    ///   the `Root` (the initial call) and `PV` (recursive calls), allowing for
    ///   slightly different behavior (e.g., root move printing, draw checks).
    /// * `pv`: A mutable reference to a `PVLine` struct. This will be updated to store the
    ///   Principal Variation (the sequence of best moves found) for the current node.
    /// * `alpha`: The lower bound of the search window (`Eval`). Represents the minimum score
    ///   that the *current player* is already guaranteed to achieve based on previously explored
    ///   moves. The function aims to find a score greater than `alpha`.
    /// * `beta`: The upper bound of the search window (`Eval`). Represents the maximum score
    ///   that the *opponent* will allow the current player to achieve. If a move results in a
    ///   score `>= beta`, the opponent would have had a better alternative earlier, so this
    ///   branch can be pruned (Beta Cutoff).
    /// * `depth`: The remaining search depth (`usize`). The search stops when `depth` reaches 0.
    ///
    /// # Returns
    ///
    /// An `Eval` representing the score of the current position from the perspective of the
    /// player whose turn it is. The score is guaranteed to be within the `(alpha, beta)`
    /// window if a cutoff occurs.
    /// - If the exact score is found, it will be between `alpha` and `beta`.
    /// - If `score >= beta` (Beta Cutoff / Fail-High), the returned score is at least `beta`.
    /// - If `score <= alpha` (Fail-Low), the returned score is at most `alpha`.
    ///
    /// Returns `Eval::DRAW` immediately if the search is stopped (`self.stop == true`).
    ///
    /// # Pruning Logic
    ///
    /// 1.  **Alpha Update:** If a move yields a `value` better than the current `alpha`, `alpha` is updated
    ///     to `value`. This means we've found a better guaranteed score for the current player.
    ///     The `pv` line is also updated with this better move sequence.
    /// 2.  **Beta Cutoff (Fail-High):** If a move yields a `value >= beta`, it means this move is "too good".
    ///     The opponent (at a higher node) would have already had a better alternative than allowing
    ///     this position. Therefore, exploring further sibling moves at this node is unnecessary.
    ///     The function immediately returns the current `best_value` (which is `>= beta`).
    fn negamax<NT: NodeType>(
        &mut self,
        tt: &TT,
        pv: &mut PVLine,
        mut alpha: Eval,
        mut beta: Eval,
        mut depth: usize,
    ) -> Eval {
        // Clear pv for new search
        pv.clear();

        if self.should_stop_search() {
            return Eval::DRAW;
        }

        let in_check = self.board.in_check();

        // --- Quiescence search in base case ---
        if depth == 0 && !in_check {
            return self.quiescence::<NT::Next>(tt, pv, alpha, beta);
        }

        if !NT::ROOT {
            // Check ply limit to prevent infinite recursion in rare cases
            if self.ply >= MAX_DEPTH as u16 && !in_check {
                return evaluate_nnue(&self.board, &mut self.nnue); // Return static eval if too deep
            }
            // Check for draws (Repetition, 50-move rule)
            if self.board.is_draw(self.ply) {
                return Eval::DRAW;
            }

            alpha = alpha.max(Eval::mated_in(self.ply));
            beta = beta.min(Eval::mate_in(self.ply + 1));

            if alpha >= beta {
                return alpha;
            }
        }

        // --- Set up Search ---
        // Make sure the depth is not going to be negative
        depth = depth.max(1);
        // Update self depth
        self.seldepth = if NT::ROOT {
            0
        } else {
            self.seldepth.max(self.ply as usize)
        };

        // Create child pv to pass to the next recursive call to negamax
        let mut child_pv = PVLine::default();

        // --- Hash Table Lookup ---
        let tt_entry = tt.get(self.board.key());
        let mut tt_move = Move::NONE;
        let mut tt_capture = false;
        self.tt_probes += 1;

        if let Some(tt_entry) = tt_entry {
            self.tt_move_hit += 1;

            let tt_value = tt_entry.value.from_tt(self.ply);

            if !NT::PV && tt_entry.depth >= depth as u8 {
                match tt_entry.bound {
                    // If we already evaluated this position at a higher depth, then we can prune this branch (Redundant).
                    TTBound::Exact => return tt_value,
                    // If we already established an upper bound for alpha, if we have some better moves then we can prune this branch.
                    TTBound::Upper if tt_value <= alpha => return tt_value,
                    // If we already established a lower bound for beta, if the opponent has some better moves then this node is too good.
                    TTBound::Lower if tt_value >= beta => return tt_value,
                    _ => {}
                }
            }

            // Update best move from hash table
            tt_move = tt_entry.best_move;
            tt_capture = self.board.is_capture(tt_move);
        }

        let eval = self.static_eval(in_check, tt_entry);

        let improving = self.improving();

        let opp_worsening = self.opp_worsening();

        // --- Set up main loop ---
        let mut best_value = -Eval::INFINITY;
        let mut best_move = Move::NONE;

        let mut move_count = 0;

        // Clear child killer moves
        self.stack[(self.ply + 2) as usize].killers.clear();
        // Get killer moves
        let killers = self.ss().killers.get();
        // Initialise move picker
        let mut move_picker = MovePicker::<false>::new(&self.board, tt_move, killers);

        // --- Main Loop ---
        while let Some(move_) = move_picker.next(&self.board, &self.stats) {
            // Update number of moves searched in this node
            move_count += 1;
            // Make move and update ply, node counters, prefetch hash entry, etc...
            self.make_move(tt, move_);
            // Remember previous node count
            let start_nodes = self.nodes;
            // Move flags
            let is_capture = move_.is_capture();
            // let is_promotion = move_.is_promotion();

            // New depth
            let new_depth = depth.max(1) - 1;

            // Recursive search
            // let value = -self.negamax::<NT::Next>(tt, &mut child_pv, -beta, -alpha, depth - 1);
            let mut value = alpha;

            // Late Move Reduction, search moves that are sufficiently far from the terminal nodes and are not tactical using a reduced depth zero window search to see if it is promising or not.
            let full_search = if !NT::PV && depth >= 2 && move_count > 1 && !is_capture {
                // Calculate dynamic depth reduction
                let r = 1 + ((move_count > 6) as usize) * depth / 3;

                let reduced_depth = new_depth.max(r) - r;

                value = -self.negamax::<NonPV>(
                    tt,
                    &mut child_pv,
                    -alpha - Eval(1),
                    -alpha,
                    reduced_depth,
                );

                value > alpha
            } else {
                !NT::PV || move_count > 1 || is_capture
            };

            // We do a zero window full depth search if the reduced depth search revealed a potentially better move,
            // or if other conditions like being in a NonPV node, searching non-first moves, are met.
            if full_search {
                value =
                    -self.negamax::<NonPV>(tt, &mut child_pv, -alpha - Eval(1), -alpha, new_depth);
            }

            // If we are current searching in a PV node, the if its the first move or further searches revealed a potentially better move,
            // we do a full search to further investigate
            if NT::PV && (move_count == 1 || value > alpha) {
                value = -self.negamax::<NT::Next>(tt, &mut child_pv, -beta, -alpha, new_depth);
            }

            // Undo move and decrement ply counter
            self.undo_move(move_);

            // Check for engine stop flag
            if self.stop {
                return Eval::DRAW;
            }

            // If this is a root node, update the node counts.
            if NT::ROOT {
                self.clock
                    .update_node_counts(move_, self.nodes - start_nodes);
            }

            // If the move we just search is better than best_value (The best we can do in this subtree), we can update best_value to be alpha.
            if value > best_value {
                best_value = value; // Update best_value
                // Alpha Update: Check if this move's score (`value`) is better than the
                // best score we are *already guaranteed* (`alpha`) from other parts of the tree.
                if value > alpha {
                    // We found a new best move sequence overall.
                    best_move = move_; // Update the best move.

                    if move_count == 1 {
                        self.best_first_move += 1;
                    }
                    self.best_move_updates += 1;

                    // Beta Cutoff (Fail-High): Check if our guaranteed score (`alpha`)
                    // meets or exceeds the opponent's limit (`beta`).
                    // This move is "too good". The opponent (at a higher node)
                    // would have already had a better alternative than allowing this position.
                    // Therefore, exploring further sibling moves at this node is unnecessary.
                    if value >= beta {
                        break;
                    }

                    alpha = value; // Update alpha: Raise the lower bound of our guaranteed score.
                    if NT::PV {
                        pv.update_line(move_, &child_pv); // Update the Principal Variation (best move sequence).
                    }
                }
            }
        }

        // Update search stack move count
        self.ss_mut().move_count = move_count as u8;
        // Update search stack in check flag
        self.ss_mut().in_check = in_check;

        // If move count is 0, it is either a stalemate or a mate in self.ply
        if move_count == 0 {
            best_value = if in_check {
                Eval::mated_in(self.ply)
            } else {
                Eval::DRAW
            };
            // If there is a new best move, and it is not a capture, update the killer move and history move table
        } else if best_move.is_valid() && !best_move.is_capture() {
            self.update_search_stats(best_move, depth);
        }

        // Write to TT, save static eval
        if !NT::ROOT {
            let bound = if best_value >= beta {
                TTBound::Lower
            } else if NT::PV && best_move.is_valid() {
                TTBound::Exact
            } else {
                TTBound::Upper
            };

            tt.write(
                self.board.key(),
                bound,
                self.ply,
                depth as u8,
                best_move,
                eval,
                best_value,
            );
        }

        best_value
    }

    /// Performs a Quiescence Search to stabilize the evaluation at tactical leaves.
    ///
    /// Extends the search beyond the nominal depth limit (`depth == 0` in `negamax`)
    /// by considering only non-quiet moves (primarily captures) until a position
    /// is reached where no immediate, significant tactical changes are likely.
    ///
    /// # Parameters
    ///
    /// * `NT`: Generic node type (usually `PV` when called from `negamax`).
    /// * `pv`: Mutable reference to a `PVLine` (less critical in qsearch but kept for consistency).
    /// * `alpha`: The lower bound of the search window.
    /// * `beta`: The upper bound of the search window.
    ///
    /// # Returns
    ///
    /// An `Eval` representing the stabilized score of the position, considering captures.
    fn quiescence<NT: NodeType>(
        &mut self,
        tt: &TT,
        pv: &mut PVLine,
        mut alpha: Eval,
        beta: Eval,
    ) -> Eval {
        self.seldepth = self.seldepth.max(self.ply as usize);

        pv.clear();

        if self.should_stop_search() {
            return Eval::DRAW;
        }

        let in_check = self.board.in_check();

        // Check ply limit to prevent infinite recursion in rare cases
        if self.ply >= MAX_DEPTH as u16 {
            return if in_check {
                -Eval::INFINITY
            } else {
                evaluate_nnue(&self.board, &mut self.nnue)
            }; // Return static eval if too deep
        }

        // Check for draws (Repetition, 50-move rule)
        if self.board.is_draw(self.ply) {
            return Eval::DRAW;
        }

        // --- Hash table probe ---
        let tt_entry = tt.get(self.board.key());
        let mut tt_move = Move::NONE;

        self.tt_probes += 1;

        if let Some(entry) = tt_entry {
            let tt_value = entry.value.from_tt(self.ply);

            if !NT::PV {
                match entry.bound {
                    TTBound::Exact => return tt_value,
                    TTBound::Upper if tt_value <= alpha => return tt_value,
                    TTBound::Lower if tt_value >= beta => return tt_value,
                    _ => {}
                }
            }

            tt_move = entry.best_move;
        }

        // --- Stand Pat Score ---
        // Get the static evaluation of the current position.
        // This score assumes no further captures are made (the "stand pat" score).
        let eval = self.static_eval(in_check, tt_entry);

        // --- Alpha-Beta Pruning based on Stand Pat ---

        // If the static evaluation is already >= beta, the opponent won't allow this position.
        // We can prune immediately, assuming the static eval is a reasonable lower bound.
        if eval >= beta {
            return beta; // Fail-High based on static eval
        }
        // If the static evaluation is better than alpha, update alpha.
        // This becomes the baseline score we need to beat with captures.
        alpha = alpha.max(eval);

        // Initialize best_value with stand_pat. We are looking for captures that improve on this.
        let mut best_value = eval;
        let mut child_pv = PVLine::default();
        let mut best_move = Move::NONE;

        // --- Generate and Explore Captures Only ---
        // The generic parameter 'true' tells MovePicker to skip quiet moves.
        let mut move_picker = MovePicker::<true>::new(&self.board, tt_move, [Move::NONE; 2]);

        while let Some(move_) = move_picker.next(&self.board, &self.stats) {
            // Make the capture
            self.make_move(tt, move_);
            // Recursive call
            let value = -self.quiescence::<NT::Next>(tt, &mut child_pv, -beta, -alpha);
            // Undo the capture
            self.undo_move(move_);

            // Check for stop signal after recursive call
            if self.stop {
                return Eval::DRAW;
            }

            // --- Alpha-Beta Update (same as negamax) ---
            best_value = best_value.max(value); // Update the best score found locally at this node.

            if value > alpha {
                best_move = move_; // Update the best move.
                pv.update_line(move_, &child_pv); // Update PV line for quiescence
                alpha = value; // Update alpha
            }

            if value >= beta {
                // Beta Cutoff (Fail-High)
                best_value = beta;
                break;
            }
        }

        // No moves left, but still in check: checkmate
        if in_check && best_value == -Eval::INFINITY {
            return Eval::mated_in(self.ply);
        }

        let bound = if best_value >= beta {
            TTBound::Lower
        } else {
            TTBound::Upper
        };

        tt.write(
            self.board.key(),
            bound,
            self.ply,
            0,
            best_move,
            eval,
            best_value,
        );

        best_value
    }

    fn make_move(&mut self, tt: &TT, move_: Move) {
        // Make move
        self.board.make_move(move_);
        // Hint at the next hash entry
        tt.prefetch(self.board.key());
        // Update search stack
        self.ss_mut().curr_move = move_;
        self.ss_mut().moved = self.board.on(move_.to());
        self.ss_mut().ply_from_null = self.ply_from_null;

        self.ply += 1;
        self.nodes += 1;
        self.ply_from_null += 1;
    }

    fn make_null_move(&mut self, tt: &TT) {
        // Make null move
        self.board.make_null_move();
        // Hint at the next hash entry
        tt.prefetch(self.board.key());
        // Update search stack
        self.ss_mut().curr_move = Move::NULL;
        self.ss_mut().moved = None;
        self.ss_mut().ply_from_null = 0;

        self.ply += 1;
        self.nodes += 1;
        self.ply_from_null += 1;
    }

    fn undo_move(&mut self, move_: Move) {
        // Undo move
        self.board.undo_move(move_);
        // Update ply
        self.ply -= 1;
        // Update ply from null
        self.ply_from_null = self.ss().ply_from_null;
    }

    fn undo_null_move(&mut self) {
        // Undo null move
        self.board.undo_null_move();
        // Update ply
        self.ply -= 1;
        // Update ply from null
        self.ply_from_null = self.ss().ply_from_null;
    }

    fn update_search_stats(&mut self, best_move: Move, depth: usize) {
        self.ss_mut().killers.update(best_move);

        let bonus = (depth * depth) as i16;

        self.stats
            .main_history
            .update(&self.board, best_move, bonus);
    }

    fn static_eval(&mut self, in_check: bool, tt_entry: Option<TTEntry>) -> Eval {
        if in_check {
            self.ss_mut().eval = -Eval::INFINITY;
            -Eval::INFINITY
        } else if let Some(tt_entry) = tt_entry {
            let tt_eval = tt_entry.eval;
            let tt_value = tt_entry.value.from_tt(self.ply);

            let eval = if tt_eval == -Eval::INFINITY {
                evaluate_nnue(&self.board, &mut self.nnue)
            } else {
                tt_eval
            };

            self.ss_mut().eval = eval;

            // If we probe the tt_entry and the tt_value is tighter than the eval, then we can use it
            match tt_entry.bound {
                // If the current node has already been searched to a higher depth, then the tt_value will be a better score.
                TTBound::Exact => tt_value,
                // If the current node has a upper bound, then if the current eval is greater than the tt_value, it means the current eval is optimistic so we should use tt_value instead
                TTBound::Upper if tt_value <= eval => tt_value,
                // If the current node has a lower bound, then if the current eval is lower than the tt_value, it means we are underestimating our opponent's response, so we should use tt_value instead
                TTBound::Lower if tt_value >= eval => tt_value,
                // If there is no stored value, or the above conditions are not met, we use eval, since it provides a tighter bound
                _ => eval,
            }
        } else {
            self.ss_mut().eval = evaluate_nnue(&self.board, &mut self.nnue);
            self.ss().eval
        }
    }

    fn improving(&self) -> bool {
        if self.ply >= 2 {
            self.ss().eval > self.ss_at(2).eval
        } else {
            false
        }
    }

    fn opp_worsening(&self) -> bool {
        if self.ply >= 1 {
            self.ss().eval > -self.ss_at(1).eval
        } else {
            false
        }
    }
}
