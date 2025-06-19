use super::init::*;
use crate::core::{Bitboard, Colour, Direction, PieceType, Square};

/******************************************\
|==========================================|
|              Type Definitions            |
|==========================================|
\******************************************/

pub(super) type AttackTable = [Bitboard; Square::NUM];

type PawnAttackTable = [[Bitboard; Square::NUM]; Colour::NUM];

pub(super) type SquarePairTable = [[Bitboard; Square::NUM]; Square::NUM];

pub(super) type DistanceTable = [[u8; Square::NUM]; Square::NUM];

/******************************************\
|==========================================|
|              Attack Tables               |
|==========================================|
\******************************************/

use Direction::*;

use super::magic::{BISHOP_MAGICS, BISHOP_TABLE, ROOK_MAGICS, ROOK_TABLE};

const PAWN_ATTACKS: PawnAttackTable = [
    init_pseudo_attacks(&[NE, NW]),
    init_pseudo_attacks(&[SE, SW]),
];

const KNIGHT_ATTACKS: AttackTable = init_pseudo_attacks(&[NNE, NNW, NEE, NWW, SEE, SWW, SSE, SSW]);

const KING_ATTACKS: AttackTable = init_pseudo_attacks(&[N, NE, NW, E, W, SE, SW, S]);

/******************************************\
|==========================================|
|              Lookup Tables               |
|==========================================|
\******************************************/

const LINE_BB: SquarePairTable = init_line_bb_table();

const BETWEEN_BB: SquarePairTable = init_between_bb_table();

const PIN_BB: SquarePairTable = init_pin_bb_table();

const CHECK_BB: SquarePairTable = init_check_bb_table();

const DIST: DistanceTable = init_dist_table();

/******************************************\
|==========================================|
|               Get Attacks                |
|==========================================|
\******************************************/

#[inline]
pub fn pawn_attack(col: Colour, sq: Square) -> Bitboard {
    unsafe {
        *PAWN_ATTACKS
            .get_unchecked(col.index())
            .get_unchecked(sq.index())
    }
}

#[inline]
pub fn knight_attack(sq: Square) -> Bitboard {
    unsafe { *KNIGHT_ATTACKS.get_unchecked(sq.index()) }
}

#[inline]
pub fn king_attack(sq: Square) -> Bitboard {
    unsafe { *KING_ATTACKS.get_unchecked(sq.index()) }
}

#[inline]
pub fn bishop_attacks(sq: Square, occ: Bitboard) -> Bitboard {
    unsafe { *BISHOP_TABLE.get_unchecked(BISHOP_MAGICS[sq.index()].index(occ)) }
}

#[inline]
pub fn rook_attacks(sq: Square, occ: Bitboard) -> Bitboard {
    unsafe { *ROOK_TABLE.get_unchecked(ROOK_MAGICS[sq.index()].index(occ)) }
}

#[inline]
pub fn queen_attacks(sq: Square, occ: Bitboard) -> Bitboard {
    bishop_attacks(sq, occ) | rook_attacks(sq, occ)
}

#[inline]
pub fn attacks(col: Colour, pt: PieceType, sq: Square, occ: Bitboard) -> Bitboard {
    match pt {
        PieceType::Pawn => pawn_attack(col, sq),
        PieceType::Knight => knight_attack(sq),
        PieceType::King => king_attack(sq),
        PieceType::Bishop => bishop_attacks(sq, occ),
        PieceType::Rook => rook_attacks(sq, occ),
        PieceType::Queen => queen_attacks(sq, occ),
    }
}

/******************************************\
|==========================================|
|           Get Bitboard Lookups           |
|==========================================|
\******************************************/

#[inline]
pub fn line_bb(from: Square, to: Square) -> Bitboard {
    LINE_BB[from.index()][to.index()]
}

#[inline]
pub fn between_bb(from: Square, to: Square) -> Bitboard {
    BETWEEN_BB[from.index()][to.index()]
}

#[inline]
pub fn pin_bb(king: Square, pinner: Square) -> Bitboard {
    PIN_BB[king.index()][pinner.index()]
}

#[inline]
pub fn check_bb(king: Square, checker: Square) -> Bitboard {
    CHECK_BB[king.index()][checker.index()]
}

#[inline]
pub fn sq_dist(sq1: Square, sq2: Square) -> u8 {
    DIST[sq1.index()][sq2.index()]
}

#[inline]
pub fn aligned(sq1: Square, sq2: Square, sq3: Square) -> bool {
    line_bb(sq1, sq2).contains(sq3)
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::utils::PRNG;

    #[test]
    fn test_init_all_attack_tables() {}

    #[test]
    fn test_pawn_attacks() {
        for sq in Square::iter() {
            let attack = pawn_attack(Colour::White, sq);
            let sq_bb = sq.bb();
            let naive_attack =
                Bitboard::shift(&sq_bb, Direction::NE) | Bitboard::shift(&sq_bb, Direction::NW);
            assert_eq!(
                attack, naive_attack,
                "White pawn attack mismatch for {:?}",
                sq
            );

            let attack = pawn_attack(Colour::Black, sq);
            let sq_bb = sq.bb();
            let naive_attack =
                Bitboard::shift(&sq_bb, Direction::SE) | Bitboard::shift(&sq_bb, Direction::SW);
            assert_eq!(
                attack, naive_attack,
                "Black pawn attack mismatch for {:?}",
                sq
            );
        }
    }

    #[test]
    fn test_knight_attacks() {
        for sq in Square::iter() {
            let attack = knight_attack(sq);
            let sq_bb = sq.bb();
            let naive_attack = Bitboard::shift(&sq_bb, Direction::NNE)
                | Bitboard::shift(&sq_bb, Direction::NNW)
                | Bitboard::shift(&sq_bb, Direction::NEE)
                | Bitboard::shift(&sq_bb, Direction::NWW)
                | Bitboard::shift(&sq_bb, Direction::SEE)
                | Bitboard::shift(&sq_bb, Direction::SWW)
                | Bitboard::shift(&sq_bb, Direction::SSE)
                | Bitboard::shift(&sq_bb, Direction::SSW);
            assert_eq!(attack, naive_attack, "Knight attack mismatch for {:?}", sq);
        }
    }

    #[test]
    fn test_king_attacks() {
        for sq in Square::iter() {
            let attack = king_attack(sq);
            let sq_bb = sq.bb();
            let naive_attack = Bitboard::shift(&sq_bb, Direction::N)
                | Bitboard::shift(&sq_bb, Direction::NE)
                | Bitboard::shift(&sq_bb, Direction::NW)
                | Bitboard::shift(&sq_bb, Direction::E)
                | Bitboard::shift(&sq_bb, Direction::W)
                | Bitboard::shift(&sq_bb, Direction::SE)
                | Bitboard::shift(&sq_bb, Direction::SW)
                | Bitboard::shift(&sq_bb, Direction::S);
            assert_eq!(attack, naive_attack, "King attack mismatch for {:?}", sq);
        }
    }

    #[test]
    fn test_bishop_attacks() {
        let mut rng = PRNG::default();

        for _ in 0..1000 {
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq);
                let attack = bishop_attacks(sq, occ);
                let naive_attack = Bitboard::attack_on_the_fly(PieceType::Bishop, sq.bb(), occ);
                assert_eq!(
                    attack, naive_attack,
                    "Bishop attack mismatch for {:?} with occ {}",
                    sq, occ
                );
            }
        }
    }

    #[test]
    fn test_rook_attacks() {
        let mut rng = PRNG::default();

        for _ in 0..1000 {
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq);
                let attack = rook_attacks(sq, occ);
                let naive_attack = Bitboard::attack_on_the_fly(PieceType::Rook, sq.bb(), occ);
                assert_eq!(
                    attack, naive_attack,
                    "Rook attack mismatch for {:?} with occ {}",
                    sq, occ
                );
            }
        }
    }

    #[test]
    fn test_queen_attacks() {
        let mut rng = PRNG::default();

        for _ in 0..1000 {
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq);
                let attack = queen_attacks(sq, occ);
                let naive_attack = Bitboard::attack_on_the_fly(PieceType::Bishop, sq.bb(), occ)
                    | Bitboard::attack_on_the_fly(PieceType::Rook, sq.bb(), occ);
                assert_eq!(
                    attack, naive_attack,
                    "Queen attack mismatch for {:?} with occ {}",
                    sq, occ
                );
            }
        }
    }

    #[test]
    fn test_line_bb_table() {
        for from in Square::iter() {
            for to in Square::iter() {
                assert_eq!(
                    line_bb(from, to),
                    line_bb(to, from),
                    "Line symmetry failed for {:?}-{:?}",
                    from,
                    to
                );

                let bb = line_bb(from, to);

                if from == to {
                    assert_eq!(
                        bb,
                        Bitboard::EMPTY,
                        "Line for same square failed {:?}",
                        from
                    );
                } else if !bb.is_empty() {
                }
            }
        }

        assert_eq!(
            line_bb(Square::A1, Square::A4),
            Bitboard::FILE_A,
            "line_bb(A1, A4)"
        );
        assert_eq!(
            line_bb(Square::H1, Square::F3),
            Bitboard::from([
                Square::H1,
                Square::G2,
                Square::F3,
                Square::E4,
                Square::D5,
                Square::C6,
                Square::B7,
                Square::A8,
            ]),
            "line_bb(H1, F3)"
        );
        assert_eq!(
            line_bb(Square::A1, Square::B3),
            Bitboard::EMPTY,
            "line_bb(A1, B3)"
        );
        assert_eq!(
            line_bb(Square::E4, Square::E4),
            Bitboard::EMPTY,
            "line_bb(E4, E4)"
        );
    }

    #[test]
    fn test_between_bb_table() {
        for from in Square::iter() {
            for to in Square::iter() {
                assert_eq!(
                    between_bb(from, to),
                    between_bb(to, from),
                    "Between symmetry failed for {:?}-{:?}",
                    from,
                    to
                );

                let bb = between_bb(from, to);

                assert!(
                    !bb.contains(from),
                    "Between contains 'from' square {:?} for {:?}-{:?}",
                    from,
                    from,
                    to
                );
                assert!(
                    !bb.contains(to),
                    "Between contains 'to' square {:?} for {:?}-{:?}",
                    to,
                    from,
                    to
                );
            }
        }

        assert_eq!(
            between_bb(Square::A1, Square::A4),
            Bitboard::from([Square::A2, Square::A3])
        );
        assert_eq!(between_bb(Square::H1, Square::F3), Square::G2.bb());
        assert_eq!(between_bb(Square::A1, Square::A2), Bitboard::EMPTY);
        assert_eq!(between_bb(Square::A1, Square::B3), Bitboard::EMPTY);
        assert_eq!(between_bb(Square::E4, Square::E4), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_bb() {
        for pinner in Square::iter() {
            for king in Square::iter() {
                let bb = pin_bb(king, pinner);

                if !bb.is_empty() {
                    assert_eq!(
                        bb,
                        between_bb(king, pinner) | pinner.bb(),
                        "Pin/Between relationship failed for pin_bb({:?}, {:?})",
                        king,
                        pinner
                    );

                    assert!(
                        !bb.contains(king),
                        "Pin mask includes king {:?} for pin_bb({:?}, {:?})",
                        king,
                        king,
                        pinner
                    );

                    assert!(
                        bb.contains(pinner),
                        "Pin mask missing pinner {:?} for pin_bb({:?}, {:?})",
                        pinner,
                        king,
                        pinner
                    );
                }
            }
        }

        assert_eq!(
            pin_bb(Square::A1, Square::E5),
            Bitboard::from([Square::B2, Square::C3, Square::D4, Square::E5]),
            "pin_bb(A1, E5)"
        );

        assert_eq!(
            pin_bb(Square::E4, Square::E1),
            Bitboard::from([Square::E3, Square::E2, Square::E1]),
            "pin_bb(E4, E1)"
        );
    }

    #[test]
    fn test_check_bb() {
        for checker in Square::iter() {
            for king in Square::iter() {
                let bb = check_bb(king, checker);

                if !bb.is_empty() {
                    if let Ok(dir) = Direction::try_from(checker, king) {
                        let between = between_bb(king, checker);
                        let behind = king.add(dir).map_or(Bitboard::EMPTY, |sq| sq.bb());
                        assert_eq!(
                            bb,
                            between | king.bb() | behind,
                            "Check/Between/Behind relationship failed for check_bb({:?}, {:?})",
                            king,
                            checker
                        );

                        assert!(
                            bb.contains(king),
                            "Check mask excludes king {:?} for check_bb({:?}, {:?})",
                            king,
                            king,
                            checker
                        );

                        assert!(
                            !bb.contains(checker),
                            "Check mask includes checker {:?} for check_bb({:?}, {:?})",
                            checker,
                            king,
                            checker
                        );
                    } else {
                        assert_eq!(
                            bb,
                            Bitboard::EMPTY,
                            "check_bb should be empty for non-linear {:?}, {:?}",
                            king,
                            checker
                        );
                    }
                }
            }
        }

        let expected = Bitboard::from([
            Square::A8,
            Square::A7,
            Square::A6,
            Square::A5,
            Square::A4,
            Square::A3,
            Square::A2,
        ]);
        assert_eq!(
            check_bb(Square::A8, Square::A1),
            expected,
            "check_bb(A8, A1)"
        );

        let expected_diag = Bitboard::from([Square::A5, Square::B6, Square::C7]);
        assert_eq!(
            check_bb(Square::B6, Square::D8),
            expected_diag,
            "check_bb(B6, D8)"
        );

        let expected_a1_a8 = Bitboard::from([
            Square::A1,
            Square::A2,
            Square::A3,
            Square::A4,
            Square::A5,
            Square::A6,
            Square::A7,
        ]);
        assert_eq!(
            check_bb(Square::A1, Square::A8),
            expected_a1_a8,
            "check_bb(A1, A8)"
        );
    }

    #[test]
    fn test_sq_dist() {
        assert_eq!(sq_dist(Square::A1, Square::A6), 5);
        assert_eq!(sq_dist(Square::E5, Square::F6), 1);
        assert_eq!(sq_dist(Square::H1, Square::A8), 7);
        assert_eq!(sq_dist(Square::C3, Square::C3), 0);
    }
}
