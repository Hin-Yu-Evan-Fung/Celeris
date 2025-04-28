use chess::Move;

use super::{Board, MoveStage, ScoredMoveList};

pub struct MovePicker<const SKIP_QUIET: bool> {
    stage: MoveStage,
    tt_move: Option<Move>,
    scored_move_list: ScoredMoveList,
    capture_end: usize,
    quiet_end: usize,
    bad_captures_start: usize,
}

impl<const SKIP_QUIET: bool> MovePicker<SKIP_QUIET> {
    pub fn new() -> Self {
        Self {
            stage: MoveStage::TTMove,
            tt_move: None,
            scored_move_list: ScoredMoveList::new(),
            capture_end: 0,
            quiet_end: 0,
            bad_captures_start: 0,
        }
    }
}

impl<const SKIP_QUIET: bool> MovePicker<SKIP_QUIET> {
    pub fn next(&mut self, board: &Board) -> Option<Move> {
        match self.stage {
            MoveStage::TTMove => {
                if let Some(move_) = self.tt_move {
                    Some(move_)
                } else {
                    self.stage = MoveStage::GenCaptures;
                    self.next(board)
                }
            }
            MoveStage::GenCaptures => {
                self.scored_move_list.generate_captures(board);
                self.capture_end = self.scored_move_list.len();
                self.bad_captures_start = self.capture_end;

                self.stage = MoveStage::GoodCaptures;
                self.next(board)
            }
            MoveStage::GoodCaptures => {
                if let Some(move_) = self.scored_move_list.next(self.capture_end) {
                    Some(move_)
                } else {
                    self.stage = if SKIP_QUIET {
                        MoveStage::BadCaptures
                    } else {
                        MoveStage::GenQuiets
                    };
                    self.next(board)
                }
            }
            MoveStage::GenQuiets => {
                self.scored_move_list.generate_quiets(board);
                self.quiet_end = self.scored_move_list.len();

                self.stage = MoveStage::Quiets;
                self.next(board)
            }
            MoveStage::Quiets => {
                if let Some(move_) = self.scored_move_list.next(self.quiet_end) {
                    Some(move_)
                } else {
                    self.stage = MoveStage::BadCaptures;
                    self.next(board)
                }
            }
            MoveStage::BadCaptures => {
                self.scored_move_list.set_index(self.bad_captures_start);
                if let Some(move_) = self.scored_move_list.next(self.capture_end) {
                    Some(move_)
                } else {
                    None
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    use chess::board::*;

    fn perft(board: &mut Board, depth: usize) -> usize {
        let mut move_picker = MovePicker::<false>::new();

        if depth == 0 {
            return 1;
        }

        let mut nodes = 0;

        while let Some(move_) = move_picker.next(board) {
            board.make_move(move_);
            nodes += perft(board, depth - 1);
            board.undo_move(move_);
        }

        nodes
    }

    #[test]
    fn test_tricky_fen_perft() {
        let mut board = Board::from_fen(TRICKY_FEN).unwrap();

        assert_eq!(perft(&mut board, 5), 193690690);
    }
}
