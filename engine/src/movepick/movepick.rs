use chess::{Move, MoveFlag, PieceType};

use crate::eval::Eval;

use super::{Board, MoveStage, ScoredMoveList};

// --- Move Ordering Scores ---
const GOOD_TACTICAL: Eval = Eval(20000);

pub struct MovePicker<const SKIP_QUIET: bool> {
    pub stage: MoveStage,
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

    fn score_captures(&mut self, board: &Board) {
        for i in 0..self.capture_end {
            let move_ = self.scored_move_list.move_list[i];

            let mut score = match move_.flag() {
                MoveFlag::EPCapture => Eval(105),
                _ => {
                    let attacker = unsafe { board.on(move_.from()).unwrap_unchecked() };
                    let captured = unsafe { board.on(move_.to()).unwrap_unchecked() };
                    Eval(100 + (captured.pt() as i16) * 100 + 5 - (attacker.pt() as i16))
                }
            };

            if move_.is_promotion() {
                score += Eval(10000 + (move_.promotion_pt() as i16) * 1000);
            }

            self.scored_move_list.set_score(i, score);
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

                self.score_captures(board);
                self.scored_move_list.partial_sort(0, self.capture_end);

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

    #[rustfmt::skip]
    const BENCH_LIST: &[(&str, usize, usize)] = &[
        ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 119060324),
        ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193690690),
        ("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 7, 178633661),
        ("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706045033),
        ("1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1", 5, 1063513), 
        ("3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", 6, 1134888), 
        ("8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1", 6, 1015133), 
        ("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 6, 1440467), 
        ("5k2/8/8/8/8/8/8/4K2R w K - 0 1", 6, 661072), 
        ("3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", 6, 803711), 
        ("r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", 4, 1274206), 
        ("r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", 4, 1720476), 
        ("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 6, 3821001), 
        ("8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", 5, 1004658), 
        ("4k3/1P6/8/8/8/8/K7/8 w - - 0 1", 6, 217342), 
        ("8/P1k5/K7/8/8/8/8/8 w - - 0 1", 6, 92683), 
        ("K1k5/8/P7/8/8/8/8/8 w - - 0 1", 6, 2217), 
        ("8/k1P5/8/1K6/8/8/8/8 w - - 0 1", 7, 567584), 
        ("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", 4, 23527), 
        ("4k3/8/8/8/8/8/8/4K2R w K - 0 1 ", 6, 764643),
        ("4k3/8/8/8/8/8/8/R3K3 w Q - 0 1 ", 6, 846648),
        ("4k2r/8/8/8/8/8/8/4K3 w k - 0 1 ", 6, 899442),
        ("r3k3/8/8/8/8/8/8/4K3 w q - 0 1 ", 6, 1001523),
        ("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 5, 674624),
        ("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 5, 15833292),
        ("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8  ", 5, 89941194),
        ("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 ", 5, 164075551),
        ("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 6, 3821001),
        ("K1k5/8/P7/8/8/8/8/8 w - - 0 1", 6, 2217),
        ("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 6, 1440467),
        ("3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", 6, 1134888),
        ("rbbknnqr/pppppppp/8/8/8/8/PPPPPPPP/RBBKNNQR w KQkq - 0 1", 6, 124381396),
        ("bnrkrnqb/pppppppp/8/8/8/8/PPPPPPPP/BNRKRNQB w KQkq - 0 1", 6, 146858295),
        ("nrbbqknr/pppppppp/8/8/8/8/PPPPPPPP/NRBBQKNR w KQkq - 0 1", 6, 97939069),
        ("bnrbnkrq/pppppppp/8/8/8/8/PPPPPPPP/BNRBNKRQ w KQkq - 0 1", 6, 145999259),
        ("rbknqnbr/pppppppp/8/8/8/8/PPPPPPPP/RBKNQNBR w KQkq - 0 1", 6, 126480040),
        ("qbrnnkbr/pppppppp/8/8/8/8/PPPPPPPP/QBRNNKBR w KQkq - 0 1", 6, 121613156),
    ];

    fn perft(board: &mut Board, depth: usize) -> usize {
        let mut move_list = MoveList::new();

        board.generate_moves::<LegalGen>(&mut move_list);

        if depth == 1 {
            return move_list.len();
        }

        let mut move_picker = MovePicker::<false>::new();

        let mut nodes = 0;

        while let Some(move_) = move_picker.next(board) {
            board.make_move(move_);
            nodes += perft(board, depth - 1);
            board.undo_move(move_);
        }

        nodes
    }

    fn perft_bench() -> bool {
        use std::time::Instant;

        let mut passed = true;
        println!("=============  START BENCH  =============");

        for (fen, depth, expected_nodes) in BENCH_LIST.iter() {
            let start = Instant::now();
            let mut board = Board::from_fen(fen).unwrap();
            let nodes = perft(&mut board, *depth);

            let status: &str = if nodes == *expected_nodes {
                "PASSED"
            } else {
                "FAILED"
            };

            if nodes != *expected_nodes {
                passed = false;
            }

            let time = start.elapsed().as_millis();

            println!(
                "status: {status}, time: {time:4}ms, Mnps: {:0.1}, Fen: {fen} )",
                (nodes as f64 / time as f64 / 1000.0)
            )
        }

        passed
    }

    #[test]
    fn test_perft() {
        perft_bench();
    }
}
