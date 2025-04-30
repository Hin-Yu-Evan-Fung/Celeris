use chess::board::{attacks, sq_dist};
use chess::{Bitboard, Castling, Colour, PieceType, Square};
use chess::{Board, Rank};

use super::{Eval, S, Score};

#[derive(Debug, Clone)]
pub struct PawnTable {
    entries: Box<[PawnEntry]>,
    mask: usize,
}

impl PawnTable {
    pub const SIZE: usize = 131072;

    pub fn new() -> Self {
        Self {
            entries: vec![PawnEntry::default(); Self::SIZE].into_boxed_slice(),
            mask: Self::SIZE - 1,
        }
    }

    #[inline(always)]
    fn index(&self, key: u64) -> usize {
        key as usize & self.mask
    }

    #[inline(always)]
    pub fn get(&mut self, board: &Board) -> &mut PawnEntry {
        let key = board.pawn_key();
        let entry = &mut self.entries[self.index(key)];
        if entry.key == key {
            entry
        } else {
            entry.key = key;
            entry.scores[Colour::White as usize] = entry.eval_pawn(Colour::White, board);
            entry.scores[Colour::Black as usize] = entry.eval_pawn(Colour::Black, board);
            entry
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PawnEntry {
    key: u64,
    pub scores: [Score; Colour::NUM],
    ksq: [Option<Square>; Colour::NUM],
    king_safety: [Score; Colour::NUM],
    castling: [Castling; Colour::NUM],
}

impl Default for PawnEntry {
    fn default() -> Self {
        Self {
            key: 0,
            scores: [Score::ZERO; Colour::NUM],
            ksq: [None; Colour::NUM],
            king_safety: [Score::ZERO; Colour::NUM],
            castling: [Castling::NONE; Colour::NUM],
        }
    }
}

const DOUBLED: Score = S!(5, 5);
const ISOLATED: Score = S!(5, 5);
const BACKWARD: Score = S!(5, 5);
const PASSED: [Score; Rank::NUM] = [
    S!(0, 0),
    S!(5, 10),
    S!(7, 10),
    S!(20, 30),
    S!(40, 40),
    S!(90, 100),
    S!(130, 150),
    S!(0, 0),
];
const CONNECTED: [i16; Rank::NUM] = [0, 0, 2, 5, 5, 10, 25, 40];
const BLOCKED: Score = S!(5, 5);
// Pawn shield penalties (applied per pawn structure defect)
const PAWN_SHIELD_MISSING: Score = S!(10, 2); // Penalty for a missing pawn in the shield zone
const PAWN_SHIELD_PUSHED_ONE: Score = S!(8, 4); // Penalty for a shield pawn pushed one square (rank+2)
const PAWN_SHIELD_PUSHED_TWO: Score = S!(15, 8); // Penalty for a shield pawn pushed > one square (rank+3 or more)
const PAWN_SHIELD_STORM_UNOPPOSED: Score = S!(15, 5); // Penalty for unopposed enemy pawn in shield zone
const PAWN_SHIELD_STORM_OPPOSED: Score = S!(10, 3); // Penalty for opposed enemy pawn in shield zone

// Open file penalties (applied per relevant open/semi-open file near the king)
const SEMI_OPEN_FILE_ADJACENT_KING: Score = S!(10, 3); // Penalty for semi-open file next to king with enemy heavy piece
const OPEN_FILE_ADJACENT_KING: Score = S!(15, 5); // Penalty for fully open file next to king with enemy heavy piece

impl PawnEntry {
    pub(super) fn eval_pawn(&mut self, us: Colour, board: &Board) -> Score {
        let opp = !us;
        let up = us.forward();
        let down = opp.forward();

        let our_pawns = board.piece_bb(us, PieceType::Pawn);
        let opp_pawns = board.piece_bb(opp, PieceType::Pawn);

        let mut score = Score::ZERO;

        our_pawns.for_each(|sq| {
            let rank = sq.relative(us).rank();

            // Flag each pawn (Doubled, Isolated, Supported, etc...)
            let blocked = (opp_pawns & sq.bb().shift(up)).is_occupied();
            let stoppers = opp_pawns & Bitboard::passed_pawn_span(us, sq);
            let doubled = (our_pawns & sq.bb().shift(down)).is_occupied();
            let neighbours = our_pawns & Bitboard::adjacent_files(sq);
            let phalanx = neighbours & sq.rank().bb();
            let support = neighbours & sq.rank().bb().shift(down);

            if doubled {
                score -= DOUBLED;
            }

            let backward = (neighbours
                & Bitboard::forward_ranks(opp, unsafe { sq.add_unchecked(up) }))
            .is_empty()
                && blocked;

            if stoppers.is_empty() {
                score += PASSED[rank as usize];
            }

            if (support | phalanx).is_occupied() {
                let v = CONNECTED[rank as usize];
                let eg_val = v * (rank as i16 - 2) / 4;
                score += S!(v, eg_val);
            } else if neighbours.is_empty() {
                score -= ISOLATED;
            } else if backward {
                score -= BACKWARD;
            }

            if support.is_empty() && doubled {
                score -= DOUBLED;
            }

            if blocked && rank >= Rank::Rank5 {
                score += BLOCKED;
            }
        });

        score
    }

    fn eval_pawn_shield(&self, board: &Board, us: Colour, ksq: Square) -> Score {
        let them = !us;

        // --- Evaluate Pawn Shield ---
        let king_shield_area = Bitboard::passed_pawn_span(us, ksq)
            & Bitboard::forward_ranks(them, Square::D5.relative(us));
        let our_pawns = board.piece_bb(us, PieceType::Pawn);
        let opp_pawns = board.piece_bb(them, PieceType::Pawn);

        // Check for pawn storms by the opponent
        let opp_attackers = opp_pawns & king_shield_area;
        let opp_blocked = opp_attackers & our_pawns.shift(us.forward());
        let mut shield_score = Score::ZERO;

        if opp_attackers.is_occupied() {
            if opp_blocked.is_occupied() {
                shield_score -= PAWN_SHIELD_STORM_OPPOSED;
            } else {
                shield_score -= PAWN_SHIELD_STORM_UNOPPOSED;
            }
        }

        // Check our shield pawns (missing or pushed too far)
        let expected_shield_rank1 = ksq.rank().bb().shift(us.forward());
        let expected_shield_rank2 = ksq.rank().bb().shift(us.forward_double());

        if (king_shield_area & our_pawns).is_empty() {
            shield_score -= PAWN_SHIELD_MISSING;
        } else if (king_shield_area & expected_shield_rank1).is_empty() {
            shield_score -= PAWN_SHIELD_PUSHED_ONE;
        } else if (king_shield_area & expected_shield_rank2).is_empty() {
            shield_score -= PAWN_SHIELD_PUSHED_TWO;
        }

        shield_score
    }

    /// Evaluates king safety for a given side ('us').
    /// Returns a Score where positive values mean safety and negative values mean danger.
    /// This score will be subtracted from the opponent's perspective in the main eval.
    fn eval_king_safety(&mut self, board: &Board, us: Colour) -> Score {
        let them = !us;
        let ksq = board.ksq(us);

        let mut safety_score = self.eval_pawn_shield(board, us, ksq);

        if board.castling().has(Castling::king_side(us)) {
            safety_score =
                safety_score.max(self.eval_pawn_shield(board, us, Square::G1.relative(us)));
        }

        if board.castling().has(Castling::queen_side(us)) {
            safety_score =
                safety_score.max(self.eval_pawn_shield(board, us, Square::C1.relative(us)));
        }

        let our_pawns = board.piece_bb(us, PieceType::Pawn);
        let mut min_pawn_dist = 6;

        if (our_pawns & attacks(us, PieceType::King, ksq, Bitboard::EMPTY)).is_occupied() {
            min_pawn_dist = 1;
        } else {
            our_pawns.for_each(|sq| {
                min_pawn_dist = min_pawn_dist.min(sq_dist(sq, ksq));
            })
        }

        if board.is_semi_open_file(us, ksq) {
            safety_score -= SEMI_OPEN_FILE_ADJACENT_KING;
        } else if board.is_open_file(us, ksq) {
            safety_score -= OPEN_FILE_ADJACENT_KING;
        }

        safety_score - S!(0, min_pawn_dist as i16)
    }

    pub fn king_safety(&mut self, board: &Board, us: Colour) -> Score {
        if self.ksq[us as usize].is_some_and(|sq| sq == board.ksq(us))
            && board.castling_side(us) == self.castling[us as usize]
        {
            self.king_safety[us as usize]
        } else {
            self.king_safety[us as usize] = self.eval_king_safety(board, us);
            self.ksq[us as usize] = Some(board.ksq(us));
            self.castling[us as usize] = board.castling_side(us);
            self.king_safety[us as usize]
        }
    }
}
