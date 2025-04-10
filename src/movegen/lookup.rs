use crate::core::{Bitboard, Castling, Colour, Direction, File, PieceType, Rank, Square};
use crate::movegen::magic::{SliderAttackTable, get_magic_tables};
use std::sync::LazyLock;

/******************************************\
|==========================================|
|              Type Definitions            |
|==========================================|
\******************************************/

/// Color index type
type ColourIndex = usize;

/// Attack table for a single piece type indexed by square
type AttackTable = [Bitboard; Square::NUM];

/// Attack table for pawns indexed by color and square
type PawnAttackTable = [[Bitboard; Square::NUM]; Colour::NUM];

/// Table mapping square pairs to bitboards
type SquarePairTable = [[Bitboard; Square::NUM]; Square::NUM];

/// Table mapping square pairs to distances
type DistanceTable = [[u8; Square::NUM]; Square::NUM];

/// Castling rights lookup table
type CastlingTable = [Castling; Square::NUM];

/// Function type for populating square pair tables
type PopulateTableFn = dyn FnMut(&mut SquarePairTable, PieceType, Square, Square);

/******************************************\
|==========================================|
|                Constants                 |
|==========================================|
\******************************************/

const BISHOP_TABLE_SIZE: usize = 0x1480; // 5248 - size of the bishop attack hash table
const ROOK_TABLE_SIZE: usize = 0x19000; // 102400 - size of the rook attack hash table

/******************************************\
|==========================================|
|              Attack Tables               |
|==========================================|
\******************************************/

use Direction::*;

/// # Pawn Attack Tables
/// - The pawn attack table is used to store the precalculated attacks for the pawn piece.
/// - For white pawns, attacks are in the NE and NW directions.
/// - For black pawns, attacks are in the SE and SW directions.
/// - The table is indexed by colour and then by square.
static PAWN_ATTACKS: LazyLock<PawnAttackTable> = LazyLock::new(|| {
    [
        init_pseudo_attacks(&[NE, NW]), // White pawn attacks (index 0)
        init_pseudo_attacks(&[SE, SW]), // Black pawn attacks (index 1)
    ]
});

/// # Knight Attack Tables
/// - The knight attack table is used to store the precalculated attacks for the knight piece.
/// - Knights move in an L-shape pattern: 2 squares in one direction and then 1 square perpendicular.
/// - The table is indexed by square, providing the attack bitboard for a knight on that square.
static KNIGHT_ATTACKS: LazyLock<AttackTable> =
    LazyLock::new(|| init_pseudo_attacks(&[NNE, NNW, NEE, NWW, SEE, SWW, SSE, SSW]));

/// # King Attack Tables
/// - The king attack table is used to store the precalculated attacks for the king piece.
/// - Kings can move one square in any of the 8 directions.
/// - The table is indexed by square, providing the attack bitboard for a king on that square.
static KING_ATTACKS: LazyLock<AttackTable> =
    LazyLock::new(|| init_pseudo_attacks(&[N, NE, NW, E, W, SE, SW, S]));

/// # Bishop Attack Tables
/// - The bishop attack table is used to store the attacks for the bishop piece.
/// - The two elements are the table and the magic entry.
/// - The table acts as a hash table for the bishop moves.
/// - The magic entry is used to generate the table key.
static BISHOP_TABLE: LazyLock<SliderAttackTable<BISHOP_TABLE_SIZE>> =
    LazyLock::new(|| get_magic_tables::<BISHOP_TABLE_SIZE>(PieceType::Bishop));

/// # Rook Attack Tables
/// - The rook attack table is used to store the attacks for the rook piece.
/// - The two elements are the table and the magic entry.
/// - The table acts as a hash table for the rook moves.
/// - The magic entry is used to generate the table key.
static ROOK_TABLE: LazyLock<SliderAttackTable<ROOK_TABLE_SIZE>> =
    LazyLock::new(|| get_magic_tables::<ROOK_TABLE_SIZE>(PieceType::Rook));

/******************************************\
|==========================================|
|              Lookup Tables               |
|==========================================|
\******************************************/

/// # Line Crossing Squares
/// - Stores the bitboard of the line crossing two squares.
static LINE_BB: LazyLock<SquarePairTable> =
    LazyLock::new(|| init_lookup_table(&mut populate_line_bb));

/// # Line Between Squares
/// - Stores the bitboard of the line between two squares.
static BETWEEN_BB: LazyLock<SquarePairTable> =
    LazyLock::new(|| init_lookup_table(&mut populate_between_bb));

/// # Pin Mask
/// - Stores the bitboard of the pin mask between two squares.
/// - The pin mask is used to determines where a pinned piece can move.
static PIN_BB: LazyLock<SquarePairTable> =
    LazyLock::new(|| init_lookup_table(&mut populate_pin_bb));

/// # Check Mask
/// - Stores the bitboard of the check mask between two squares.
/// - The check mask is used to determine where a checked king can move.
static CHECK_BB: LazyLock<SquarePairTable> =
    LazyLock::new(|| init_lookup_table(&mut populate_check_bb));

/// # Castling Rights Table
/// - Stores the changes in castling rights for a square (if the king enters or exits the square)
static CASTLING_RIGHTS: LazyLock<CastlingTable> = LazyLock::new(|| init_castling_rights_table());

/// # Square distance
/// - Stores the maximum norm of the difference of two squares
static DIST: LazyLock<DistanceTable> = LazyLock::new(|| init_dist_table());

/******************************************\
|==========================================|
|              Initialisation              |
|==========================================|
\******************************************/

/// # Init all attack tables
/// - Forces the initialisation of all attack tables.
/// - Call this function during engine setup to precompute all tables.
/// - This avoids lazy initialisation during search, which could cause timing issues.
pub fn init_all_tables() {
    // Initialize the attack tables for all pieces
    let _ = &*PAWN_ATTACKS;
    let _ = &*KNIGHT_ATTACKS;
    let _ = &*KING_ATTACKS;
    let _ = &*BISHOP_TABLE;
    let _ = &*ROOK_TABLE;
    let _ = &*LINE_BB;
    let _ = &*BETWEEN_BB;
    let _ = &*PIN_BB;
    let _ = &*CHECK_BB;
    let _ = &*CASTLING_RIGHTS;
    let _ = &*DIST;
}

// # Get line between two squares
fn populate_line_bb(table: &mut SquarePairTable, pt: PieceType, from: Square, to: Square) {
    let from_bb: Bitboard = from.into();
    let to_bb: Bitboard = to.into();
    let from_ray = slider_attack(pt, from, Bitboard::EMPTY);
    let to_ray = slider_attack(pt, to, Bitboard::EMPTY);
    table[from as usize][to as usize] = (from_ray & to_ray) | (from_bb | to_bb);
}

// # Get the line between two squares
fn populate_between_bb(table: &mut SquarePairTable, pt: PieceType, from: Square, to: Square) {
    let from_ray = slider_attack(pt, from, to.into());
    let to_ray = slider_attack(pt, to, from.into());
    table[from as usize][to as usize] = from_ray & to_ray;
}

// # Get the pin mask between two squares
fn populate_pin_bb(table: &mut SquarePairTable, pt: PieceType, from: Square, to: Square) {
    let from_ray = slider_attack(pt, from, to.into());
    let to_ray = slider_attack(pt, to, from.into());
    table[from as usize][to as usize] = (from_ray & to_ray) | Bitboard::from(to);
}

// # Get the check mask between two squares
fn populate_check_bb(table: &mut SquarePairTable, pt: PieceType, from: Square, to: Square) {
    let from_ray = slider_attack(pt, from, to.into());
    let to_ray = slider_attack(pt, to, from.into());
    let dir = Direction::try_from((to, from)).ok();
    let behind_bb = if dir != None && from + dir.unwrap() != None {
        Bitboard::from((from + dir.unwrap()).unwrap())
    } else {
        Bitboard::EMPTY
    };
    table[from as usize][to as usize] = (from_ray & to_ray) | Bitboard::from(from) | behind_bb;
}

/// # Initialise Pseudo Attacks
/// - Initializes the pseudo attacks for the given directions.
/// - This is used for pawn, knight and king attacks.
/// - "Pseudo" attacks means these are the attacks without considering blocking pieces.
/// - This is sufficient for non-sliding pieces.
///
/// # Arguments
/// - `dirs`: The directions to calculate attacks for
///
/// # Returns
/// - Array of bitboards indexed by square, each containing the attack pattern
fn init_pseudo_attacks(dirs: &[Direction]) -> AttackTable {
    let mut attacks = [Bitboard::EMPTY; Square::NUM];
    for sq in Square::iter() {
        let sq_idx = sq as usize;
        let sq_bb = Bitboard::from(sq);

        for &dir in dirs {
            attacks[sq_idx] |= Bitboard::shift(&sq_bb, dir);
        }
    }
    attacks
}

/// Initialize a lookup table for square pairs
///
/// # Arguments
/// - `populate_fn`: Function that calculates the value for each square pair
///
/// # Returns
/// - A fully initialized square pair table
fn init_lookup_table(populate_fn: &mut PopulateTableFn) -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    for pt in [PieceType::Bishop, PieceType::Rook] {
        for from in Square::iter() {
            slider_attack(pt, from, Bitboard::EMPTY).for_each(|to| {
                populate_fn(&mut table, pt, from, to);
            });
        }
    }

    table
}

/// # Init Dist Table
///
/// Initializes the distance table that stores the maximum norm (Chebyshev distance)
/// between each pair of squares on the board.
///
/// The Chebyshev distance is the maximum of the rank distance and file distance.
/// This distance corresponds to the minimum number of king moves required to travel
/// from one square to another.
///
/// # Returns
/// - A 2D array where `table[sq1][sq2]` contains the distance between `sq1` and `sq2`
fn init_dist_table() -> [[u8; Square::NUM]; Square::NUM] {
    let mut table = [[0u8; Square::NUM]; Square::NUM];

    for sq1 in Square::iter() {
        for sq2 in Square::iter() {
            table[sq1 as usize][sq2 as usize] =
                std::cmp::max(Square::rank_dist(sq1, sq2), Square::file_dist(sq1, sq2));
        }
    }

    table
}

/// # Init Castling Rights Table
///
/// Initializes the castling rights table that stores which castling rights
/// are lost when a piece moves from or to a specific square.
///
/// The table is indexed by square and contains a `Castling` value with bits
/// representing the castling rights that should be removed. For example, moving
/// a piece to or from E1 (white king's starting square) would remove all white castling rights.
///
/// # Returns
/// - An array where `table[sq]` contains the castling rights that should be removed
fn init_castling_rights_table() -> [Castling; Square::NUM] {
    use Square::*;

    let mut table = [Castling::ALL; Square::NUM];
    // Remove White castling rights if the king is moved
    table[E1 as usize].remove(Castling::WHITE_CASTLING);
    // Remove Black castling rights if the king is moved
    table[E8 as usize].remove(Castling::BLACK_CASTLING);
    // Remove White King side castling rights if the rook on H1 is moved
    table[H1 as usize].remove(Castling::WK);
    // Remove White Queen side castling rights if the rook on A1 is moved
    table[A1 as usize].remove(Castling::WQ);
    // Remove Black King side castling rights if the rook on H8 is moved
    table[H8 as usize].remove(Castling::BK);
    // Remove Black Queen side castling rights if the rook on A8 is moved
    table[A8 as usize].remove(Castling::BQ);

    table
}

/******************************************\
|==========================================|
|               Get Attacks                |
|==========================================|
\******************************************/

/// # Get Pawn Attacks
/// - Gets the attacks for a pawn of the given colour on the given square.
/// - The attacks are the squares that a pawn of the given colour can capture on.
/// - For white pawns, these are the squares diagonally forward (NE, NW).
/// - For black pawns, these are the squares diagonally backward (SE, SW).
///
/// # Arguments
/// - `col`: The colour of attacking side
/// - `sq`: The square to get the attacks for
///
/// # Returns
/// - Bitboard with bits set for all squares the pawn attacks
pub fn pawn_attack(col: Colour, sq: Square) -> Bitboard {
    let col_idx = col as ColourIndex;
    let sq_idx = sq as usize;

    PAWN_ATTACKS[col_idx][sq_idx]
}

/// # Get Leaper Piece Attacks
/// - Gets the attacks for the given non-sliding piece type and square.
/// - This is used for knight and king pieces, which "leap" to their destination.
/// - Leapers can move to their destination squares regardless of intervening pieces.
///
/// # Arguments
/// - `pt`: The piece type to get the attacks for (Knight or King)
/// - `sq`: The square to get the attacks for
///
/// # Returns
/// - Bitboard with bits set for all squares the piece attacks
///
/// # Panics
/// - Panics with `unreachable!()` if given a piece type other than Knight or King
pub fn leaper_attack(pt: PieceType, sq: Square) -> Bitboard {
    let sq_idx = sq as usize;

    match pt {
        PieceType::Knight => KNIGHT_ATTACKS[sq_idx],
        PieceType::King => KING_ATTACKS[sq_idx],
        _ => unreachable!("Only Knight and King are supported by leaper_attack"),
    }
}

/// # Get Slider Piece Attacks
/// - Gets the attacks for the given piece type and square.
/// - This is used for bishop, rook, queen.
///
/// # Arguments
/// - `pt`: The piece type to get the attacks for
/// - `sq`: The square to get the attacks for
/// - `occ`: The occupancy bitboard to limit the attacks
pub fn slider_attack(pt: PieceType, sq: Square, occ: Bitboard) -> Bitboard {
    match pt {
        PieceType::Bishop => BISHOP_TABLE.get_entry(sq, occ),
        PieceType::Rook => ROOK_TABLE.get_entry(sq, occ),
        PieceType::Queen => BISHOP_TABLE.get_entry(sq, occ) | ROOK_TABLE.get_entry(sq, occ),
        _ => unreachable!(),
    }
}

/******************************************\
|==========================================|
|           Get Bitboard Lookups           |
|==========================================|
\******************************************/

/// # Get Line Crossing Squares
///
/// Gets the line crossing two squares, including the squares themselves.
/// If the squares are not on the same rank, file, or diagonal, returns an empty bitboard.
/// This is useful for determining if a line exists between two squares, which is
/// needed for checking pins, rays, and other line-based chess tactics.
///
/// # Arguments
/// - `from`: The source square
/// - `to`: The destination square
///
/// # Returns
/// - Bitboard with bits set for all squares on the line, including the source and destination squares
#[inline]
pub fn line_bb(from: Square, to: Square) -> Bitboard {
    LINE_BB[from as usize][to as usize]
}

/// # Get Line Between Squares
///
/// Gets the line between two squares, excluding the squares themselves.
/// If the squares are not on the same rank, file, or diagonal, returns an empty bitboard.
/// This is useful for determining if there are pieces between two squares, which is
/// needed for validating moves and checking blockades.
///
/// # Arguments
/// - `from`: The source square
/// - `to`: The destination square
///
/// # Returns
/// - Bitboard with bits set for all squares between the source and destination, excluding endpoints
#[inline]
pub fn between_bb(from: Square, to: Square) -> Bitboard {
    BETWEEN_BB[from as usize][to as usize]
}

/// # Get Pin Mask
///
/// Gets the pin mask between two squares.
/// The pin mask is used to determine where a pinned piece can legally move.
/// A piece is pinned if moving it would expose the king to check from a sliding piece.
/// The pin mask is the line between the king and the pinning piece, including the pinning piece.
///
/// # Arguments
/// - `from`: The source square (typically the king's position)
/// - `to`: The destination square (typically the pinning piece's position)
///
/// # Returns
/// - Bitboard with bits set for the line from the source to the destination, including endpoints
#[inline]
pub fn pin_bb(from: Square, to: Square) -> Bitboard {
    PIN_BB[from as usize][to as usize]
}

/// # Get Check Mask
///
/// Gets the check mask between two squares.
/// The check mask is used to determine which squares can block a check.
/// When a king is in check from a sliding piece, any piece can block the check
/// by moving onto a square between the king and the checking piece.
///
/// # Arguments
/// - `from`: The source square (typically the king's position)
/// - `to`: The destination square (typically the checking piece's position)
///
/// # Returns
/// - Bitboard with bits set for all squares that would block the check
#[inline]
pub fn check_bb(from: Square, to: Square) -> Bitboard {
    CHECK_BB[from as usize][to as usize]
}

/// # Get Castling Rights Mask
///
/// Gets the castling rights that would be removed if a piece moves to or from the specified square.
/// This is used to update castling rights during move generation and execution.
/// Moving a piece to or from certain squares (like a king or rook's starting position)
/// removes specific castling rights.
///
/// # Arguments
/// - `from`: The square to check for castling rights implications
///
/// # Returns
/// - Castling rights that would be removed if a piece moves to or from this square
#[inline]
pub fn castling_rights(from: Square) -> Castling {
    CASTLING_RIGHTS[from as usize]
}

/// # Get Square Distance
///
/// Gets the Chebyshev distance (maximum norm) between two squares.
/// The Chebyshev distance is the maximum of the rank and file distances.
/// This distance corresponds to the minimum number of king moves required to travel
/// from one square to another. It is also known as the "king's move distance".
///
/// # Arguments
/// - `sq1`: The first square
/// - `sq2`: The second square
///
/// # Returns
/// - The Chebyshev distance between the two squares (ranging from 0 to 7)
///
/// # Examples
/// ```rust,no_run
/// use sophos::{Square, dist};
///
/// let a1 = Square::A1;
/// let h8 = Square::H8;
/// let distance = dist(a1, h8); // 7 (maximum of rank distance 7 and file distance 7)
///
/// let e4 = Square::E4;
/// let c6 = Square::C6;
/// let distance = dist(e4, c6); // 2 (maximum of rank distance 2 and file distance 2)
/// ```
#[inline]
pub fn dist(sq1: Square, sq2: Square) -> u8 {
    DIST[sq1 as usize][sq2 as usize]
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::movegen::{attacks_on_the_fly, init_all_tables};
    use crate::utils::PRNG;

    #[test]
    fn test_init_all_attack_tables() {
        init_all_tables();
    }

    #[test]
    fn test_pawn_attacks() {
        for sq in Square::iter() {
            let attack = pawn_attack(Colour::White, sq);
            let sq_bb = Bitboard::from(sq);
            let naive_attack =
                Bitboard::shift(&sq_bb, Direction::NE) | Bitboard::shift(&sq_bb, Direction::NW);
            assert_eq!(attack, naive_attack);

            let attack = pawn_attack(Colour::Black, sq);
            let sq_bb = Bitboard::from(sq);
            let naive_attack =
                Bitboard::shift(&sq_bb, Direction::SE) | Bitboard::shift(&sq_bb, Direction::SW);
            assert_eq!(attack, naive_attack);
        }
    }

    #[test]
    fn test_knight_attacks() {
        for sq in Square::iter() {
            let attack = leaper_attack(PieceType::Knight, sq);
            let sq_bb = Bitboard::from(sq);
            let naive_attack = Bitboard::shift(&sq_bb, Direction::NNE)
                | Bitboard::shift(&sq_bb, Direction::NNW)
                | Bitboard::shift(&sq_bb, Direction::NEE)
                | Bitboard::shift(&sq_bb, Direction::NWW)
                | Bitboard::shift(&sq_bb, Direction::SEE)
                | Bitboard::shift(&sq_bb, Direction::SWW)
                | Bitboard::shift(&sq_bb, Direction::SSE)
                | Bitboard::shift(&sq_bb, Direction::SSW);
            assert_eq!(attack, naive_attack);
        }
    }

    #[test]
    fn test_king_attacks() {
        for sq in Square::iter() {
            let attack = leaper_attack(PieceType::King, sq);
            let sq_bb = Bitboard::from(sq);
            let naive_attack = Bitboard::shift(&sq_bb, Direction::N)
                | Bitboard::shift(&sq_bb, Direction::NE)
                | Bitboard::shift(&sq_bb, Direction::NW)
                | Bitboard::shift(&sq_bb, Direction::E)
                | Bitboard::shift(&sq_bb, Direction::W)
                | Bitboard::shift(&sq_bb, Direction::SE)
                | Bitboard::shift(&sq_bb, Direction::SW)
                | Bitboard::shift(&sq_bb, Direction::S);
            assert_eq!(attack, naive_attack);
        }
    }

    #[test]
    fn test_bishop_attacks() {
        let mut rng = PRNG::default();

        for _ in 0..5000 {
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq);
                let attack = slider_attack(PieceType::Bishop, sq, occ);
                let naive_attack = attacks_on_the_fly(PieceType::Bishop, sq, occ);
                assert_eq!(attack, naive_attack);
            }
        }
    }

    #[test]
    fn test_rook_attacks() {
        let mut rng = PRNG::default();

        for _ in 0..5000 {
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq);
                let attack = slider_attack(PieceType::Rook, sq, occ);
                let naive_attack = attacks_on_the_fly(PieceType::Rook, sq, occ);
                assert_eq!(attack, naive_attack);
            }
        }
    }

    #[test]
    fn test_queen_attacks() {
        let mut rng = PRNG::default();

        for _ in 0..5000 {
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq);
                let attack = slider_attack(PieceType::Queen, sq, occ);
                let naive_attack = attacks_on_the_fly(PieceType::Bishop, sq, occ)
                    | attacks_on_the_fly(PieceType::Rook, sq, occ);
                assert_eq!(attack, naive_attack);
            }
        }
    }

    #[test]
    fn test_line_bb_table() {
        for from in Square::iter() {
            for to in Square::iter() {
                assert_eq!(line_bb(from, to), line_bb(to, from));

                let bb = line_bb(from, to);

                if !bb.is_empty() {
                    assert_eq!(line_bb(from, to).get(from), true);
                    assert_eq!(line_bb(from, to).get(to), true);
                }
            }
        }
    }

    #[test]
    fn test_between_bb_table() {
        for from in Square::iter() {
            for to in Square::iter() {
                assert_eq!(between_bb(from, to), between_bb(to, from));

                let bb = between_bb(from, to);

                if !bb.is_empty() {
                    assert_eq!(between_bb(from, to).get(from), false);
                    assert_eq!(between_bb(from, to).get(to), false);
                }
            }
        }
    }

    #[test]
    fn test_pin_bb() {
        for from in Square::iter() {
            for to in Square::iter() {
                let bb = pin_bb(from, to);

                assert_eq!(pin_bb(to, from) & bb, between_bb(from, to));

                if !bb.is_empty() {
                    assert_eq!(pin_bb(from, to).get(from), false);
                    assert_eq!(pin_bb(from, to).get(to), true);
                }
            }
        }
    }

    #[test]
    fn test_check_bb() {
        for from in Square::iter() {
            for to in Square::iter() {
                let bb = check_bb(from, to);

                assert_eq!(check_bb(to, from) & bb, between_bb(from, to));

                if !bb.is_empty() {
                    assert_eq!(bb.get(from), true);
                    assert_eq!(bb.get(to), false);
                    match Direction::try_from((to, from)).ok() {
                        Some(dir) => {
                            if (from + dir) != None {
                                assert!(bb.get((from + dir).unwrap()));
                            } else {
                                assert!(true);
                            }
                        }
                        None => assert!(true),
                    }
                }
            }
        }
    }

    #[test]
    fn test_sq_dist() {
        assert_eq!(dist(Square::A1, Square::A6), 5);
        assert_eq!(dist(Square::E5, Square::F6), 1);
    }
}
