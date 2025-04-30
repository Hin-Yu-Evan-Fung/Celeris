use chess::Board;
use chess::{Bitboard, Castling, Colour, PieceType, Square};

use super::Score;

pub struct PawnTable {
    entries: Box<[PawnEntry; Self::SIZE]>,
    mask: usize,
}

impl PawnTable {
    pub const SIZE: usize = 131072;

    pub fn new() -> Self {
        Self {
            entries: Box::new([PawnEntry::default(); Self::SIZE]),
            mask: Self::SIZE - 1,
        }
    }

    #[inline(always)]
    fn index(&self, key: u64) -> usize {
        key as usize & self.mask
    }

    #[inline(always)]
    pub fn get(&mut self, key: u64, board: &Board) -> &PawnEntry {
        let entry = &mut self.entries[self.index(key)];
        if entry.key == key {
            entry
        } else {
            entry.key = key;
            entry.scores[Colour::White as usize] = Score::ZERO;
            entry.scores[Colour::Black as usize] = Score::ZERO;
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
    scores: [Score; Colour::NUM],
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
    pub(super) fn eval_pawn(&mut self, us: Colour, board: &Board) {
        let opp = !us;
        let our_pawns = board.piece_bb(us, PieceType::Pawn);
        let opp_pawns = board.piece_bb(opp, PieceType::Pawn);

        let mut score = Score::ZERO;

        self.attacks[us as usize] = Bitboard::pawn_attacks(us, our_pawns);

        our_pawns.for_each(|sq| {
            let rank = sq.relative(us).rank();

            // Flag each pawn (Doubled, Isolated, Supported, etc...)
            let opposed = opp_pawns & Bitboard::forward_file(us, sq);
        });
    }
}
