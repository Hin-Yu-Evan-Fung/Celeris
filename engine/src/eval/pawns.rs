use chess::{Bitboard, Castling, Colour, PieceType, Square, board};
use chess::{Board, File, Rank};

use super::{E, Eval, S, Score};

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
    pub fn get(&mut self, board: &Board) -> &PawnEntry {
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
    attacks: [Bitboard; Colour::NUM],
    ksq: [Option<Square>; Colour::NUM],
    passed: [Bitboard; Colour::NUM],
    pub scores: [Score; Colour::NUM],
    king_safety: [Score; Colour::NUM],
    castling: [Castling; Colour::NUM],
}

impl Default for PawnEntry {
    fn default() -> Self {
        Self {
            key: 0,
            attacks: [Bitboard::EMPTY; Colour::NUM],
            ksq: [None; Colour::NUM],
            passed: [Bitboard::EMPTY; Colour::NUM],
            scores: [Score::ZERO; Colour::NUM],
            king_safety: [Score::ZERO; Colour::NUM],
            castling: [Castling::NONE; Colour::NUM],
        }
    }
}

impl PawnEntry {
    pub const DOUBLED: Score = S!(5, 5);
    pub const ISOLATED: Score = S!(5, 5);
    pub const BACKWARD: Score = S!(5, 5);
    pub const CONNECTED: [i16; Rank::NUM] = [0, 0, 2, 5, 5, 10, 25, 40];
    pub const BLOCKED: Score = S!(5, 5);

    pub(super) fn eval_pawn(&mut self, us: Colour, board: &Board) -> Score {
        let opp = !us;
        let up = us.forward();
        let down = opp.forward();

        let our_pawns = board.piece_bb(us, PieceType::Pawn);
        let opp_pawns = board.piece_bb(opp, PieceType::Pawn);

        let mut score = Score::ZERO;

        self.attacks[us as usize] = Bitboard::pawn_attacks(us, our_pawns);

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
                score -= Self::DOUBLED;
            }

            let backward = (neighbours
                & Bitboard::forward_ranks(opp, unsafe { sq.add_unchecked(up) }))
            .is_empty()
                && blocked;

            if stoppers.is_empty() {
                self.passed[us as usize] |= sq.bb();
            }

            if (support | phalanx).is_occupied() {
                let v = Self::CONNECTED[rank as usize];
                let eg_val = v * (rank as i16 - 2) / 4;
                score += S!(v, eg_val);
            } else if neighbours.is_empty() {
                score -= Self::ISOLATED;
            } else if backward {
                score -= Self::BACKWARD;
            }

            if support.is_empty() && doubled {
                score -= Self::DOUBLED;
            }

            if blocked && rank >= Rank::Rank5 {
                score += Self::BLOCKED;
            }
        });

        score
    }

    fn relavant_files(ksq: Square) -> impl Iterator<Item = File> {
        let center = if ksq.file() <= File::FileB {
            File::FileB as u8
        } else if ksq.file() >= File::FileG {
            File::FileG as u8
        } else {
            ksq.file() as u8
        };

        // File guaranteed to be in range
        unsafe {
            [
                File::from_unchecked(center - 1),
                File::from_unchecked(center),
                File::from_unchecked(center + 1),
            ]
            .into_iter()
        }
    }

    pub fn eval_shelter(&mut self, us: Colour, board: &Board) -> Score {
        Score::ZERO
    }
}
