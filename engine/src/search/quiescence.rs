use std::future;

use chess::Move;

use crate::{
    Depth, Interface, SEE_PIECE_VALUES, SearchWorker,
    constants::{CONT_HIST_SIZE, MAX_DEPTH},
    eval::Eval,
    movepick::MovePicker,
    search::{PVLine, stack::SearchStackEntry},
    see,
};

use super::{NodeType, TT, tt::TTBound, utils::*};

impl SearchWorker {
    pub(super) fn quiescence<NT: NodeType>(
        &mut self,
        tt: &TT,
        pv: &mut PVLine,
        mut alpha: Eval,
        beta: Eval,
    ) -> Eval {
        self.seldepth = self.seldepth.max(self.ply as Depth);

        pv.clear();

        if self.should_stop_search() {
            return Eval::DRAW;
        }

        let in_check = self.board.in_check();

        // Check ply limit to prevent infinite recursion in rare cases
        if self.ply >= MAX_DEPTH as u16 {
            // evaluate_nnue(&self.board, &mut self.nnue)
            return if in_check {
                Eval::DRAW
            } else {
                self.evaluate()
            };
        }

        // Check for draws (Repetition, 50-move rule)
        if self.board.is_draw(self.ply_from_null) {
            return Eval::DRAW;
        }

        // --- Hash table probe ---
        let tt_entry = tt.get(self.board.key());
        let mut tt_move = Move::NONE;
        let mut tt_bound = TTBound::None;
        let mut tt_value = -Eval::INFINITY;
        let mut tt_eval = -Eval::INFINITY;

        if let Some(tt_entry) = tt_entry {
            tt_value = tt_entry.value().from_tt(self.ply);

            if !NT::PV && can_use_tt_value(tt_entry.bound, tt_value, alpha, beta) {
                return tt_value;
            }

            // Update best move from hash table
            tt_bound = tt_entry.bound;
            tt_move = tt_entry.best_move;
            tt_eval = tt_entry.eval();
        }

        // --- Stand Pat Score ---
        // Get the static evaluation of the current position.
        // This score assumes no further captures are made (the "stand pat" score).
        // Initialize best_value with stand_pat. We are looking for captures that improve on this.
        let mut best_value;
        let mut child_pv = PVLine::default();
        let mut best_move = Move::NONE;
        let mut raw_value = -Eval::INFINITY;
        let mut futility = -Eval::INFINITY;
        let correction_value = self.stats.crt.get(&self.board, Move::NONE);
        if in_check {
            best_value = -Eval::INFINITY;
        } else {
            best_value = if tt_entry.is_some() {
                raw_value = if tt_eval.is_valid() {
                    tt_eval
                } else {
                    self.evaluate()
                };

                self.ss_at_mut(0).eval = (raw_value + correction_value / Eval(100))
                    .clamp(-Eval::MATE_BOUND, Eval::MATE_BOUND);

                // TT value can be used as a better position evaluation
                if !tt_value.is_terminal() && can_use_tt_value(tt_bound, tt_value, alpha, beta) {
                    tt_value
                } else {
                    self.ss_at_mut(0).eval
                }
            } else {
                raw_value = self.evaluate();
                self.ss_at_mut(0).eval = (raw_value + correction_value / Eval(100))
                    .clamp(-Eval::MATE_BOUND, Eval::MATE_BOUND);

                self.ss_at_mut(0).eval
            };

            // --- Alpha-Beta Pruning based on Stand Pat ---
            // If the static evaluation is already >= beta, the opponent won't allow this position.
            // We can prune immediately, assuming the static eval is a reasonable lower bound.
            if best_value >= beta {
                return beta;
            }

            // If the static evaluation is better than alpha, update alpha.
            // This becomes the baseline score we need to beat with captures.
            alpha = alpha.max(best_value);

            // Update futility value
            futility = best_value + Eval(350);
        }

        // --- Generate and Explore Captures Only ---
        let ss_buffer = [SearchStackEntry::default(); CONT_HIST_SIZE];

        // The generic parameter 'true' tells MovePicker to skip quiet moves.
        let mut move_picker =
            MovePicker::<true>::new(&self.board, tt_move, [Move::NONE; 2], Move::NONE);

        while let Some(move_) = move_picker.next(&self.board, &self.stats, &ss_buffer) {
            // --- QS Pruning ---
            if !best_value.is_terminal() {
                // --- Futility Pruning ---
                // If the move does not win material and the current static eval is pessimistic enough,
                // then we ignore the move
                let mut piece_value = Eval::ZERO;
                if move_.is_capture() {
                    piece_value = unsafe {
                        SEE_PIECE_VALUES[self.board.on_unchecked(move_.to()).pt().index()]
                    }
                };

                if in_check && futility <= alpha && !see(&self.board, move_, Eval(1)) {
                    best_value = best_value.max(futility);
                    continue;
                }

                // --- SEE Pruning ---
                if !see(&self.board, move_, Eval(-30)) {
                    continue;
                }
            }

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

            // If the move we just search is better than best_value (The best we can do in this subtree), we can update best_value to be alpha.
            if value > best_value {
                // Update best_value
                best_value = value;
                // Alpha Update: Check if this move's score (`value`) is better than the
                // best score we are *already guaranteed* (`alpha`) from other parts of the tree.
                if value > alpha {
                    // We found a new best move sequence overall.
                    best_move = move_; // Update the best move.

                    if NT::PV {
                        pv.update_line(move_, &child_pv); // Update the Principal Variation (best move sequence).
                    }

                    // Beta Cutoff (Fail-High): Check if our guaranteed score (`alpha`)
                    // meets or exceeds the opponent's limit (`beta`).
                    // This move is "too good". The opponent (at a higher node)
                    // would have already had a better alternative than allowing this position.
                    // Therefore, exploring further sibling moves at this node is unnecessary.
                    if value >= beta {
                        break;
                    }

                    alpha = value; // Update alpha: Raise the lower bound of our guaranteed score.
                }
            }
        }

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
            raw_value,
            best_value,
        );

        best_value
    }
}
