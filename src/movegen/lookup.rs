//! # Module: `lookup`
//!
//! This module provides precomputed lookup tables and functions for efficient move generation in a chess engine.
//! It includes tables for pawn, knight, king, bishop, rook, and queen attacks, as well as tables for lines,
//! between squares, pin masks, check masks, castling rights, and distances.
//!
//! ## Overview
//!
//! The module is designed to optimize the performance of the chess engine by precomputing various
//! lookup tables that are frequently used during move generation and board evaluation. These tables
//! are initialized once at the start of the program and then used throughout the engine's operation.
//!
//! ## Key Components
//!
//! - **Attack Tables**: Precomputed attack patterns for each piece type.
//!   - `PAWN_ATTACKS`: Attacks for pawns, indexed by color and square.
//!   - `KNIGHT_ATTACKS`: Attacks for knights, indexed by square.
//!   - `KING_ATTACKS`: Attacks for kings, indexed by square.
//!   - `BISHOP_TABLE`: Attacks for bishops, using magic bitboards.
//!   - `ROOK_TABLE`: Attacks for rooks, using magic bitboards.
//! - **Lookup Tables**: Tables for various board relationships.
//!   - `LINE_BB`: Bitboards representing lines between squares (inclusive).
//!   - `BETWEEN_BB`: Bitboards representing squares between two squares (exclusive).
//!   - `PIN_BB`: Bitboards representing potential pin rays.
//!   - `CHECK_BB`: Bitboards representing squares that can resolve a check.
//!   - `CASTLING_RIGHTS`: Castling rights that are removed if a piece moves to or from a square.
//!   - `DIST`: Chebyshev distances between squares.
//! - **Initialization**: Functions to initialize the tables.
//!   - `init_all_tables`: Initializes all lookup tables.
//!   - `init_pseudo_attacks`: Initializes attack tables for non-sliding pieces.
//! - **Attack Functions**: Functions to retrieve attack information.
//!   - `pawn_attack`: Gets the attack bitboard for a pawn.
//!   - `leaper_attack`: Gets the attack bitboard for a knight or king.
//!   - `slider_attack`: Gets the attack bitboard for a bishop, rook, or

use crate::core::{Bitboard, Castling, Colour, Direction, File, PieceType, Rank, Square};
// Make get_magic_tables pub(super) or pub(crate) if needed here, otherwise keep it private/pub(super) in magic.rs
// Assuming get_magic_tables is pub(super) in magic.rs and accessible here.
use super::init::*;
use super::magic::{SliderAttackTable, get_magic_tables}; // Adjusted import visibility assumption
use std::sync::LazyLock;

/******************************************\
|==========================================|
|              Type Definitions            |
|==========================================|
\******************************************/

// Type aliases remain internal implementation details

/// Attack table for a single piece type indexed by square
type AttackTable = [Bitboard; Square::NUM];
/// Attack table for pawns indexed by colour and square
type PawnAttackTable = [[Bitboard; Square::NUM]; Colour::NUM];
/// Table mapping square pairs to bitboards
pub(super) type SquarePairTable = [[Bitboard; Square::NUM]; Square::NUM];
/// Table mapping square pairs to distances
pub(super) type DistanceTable = [[u8; Square::NUM]; Square::NUM];
/// Castling rights lookup table
pub(super) type CastlingTable = [Castling; Square::NUM];
/// Function type for populating square pair tables
pub(super) type PopulateTableFn = dyn FnMut(&mut SquarePairTable, PieceType, Square, Square);

/******************************************\
|==========================================|
|                Constants                 |
|==========================================|
\******************************************/

// Constants remain internal
const BISHOP_TABLE_SIZE: usize = 0x1480; // 5248 - size of the bishop attack hash table
const ROOK_TABLE_SIZE: usize = 0x19000; // 102400 - size of the rook attack hash table

/******************************************\
|==========================================|
|              Attack Tables               |
|==========================================|
\******************************************/

use Direction::*;

/// Precomputed pawn attacks, indexed by `[Colour][Square]`.
static PAWN_ATTACKS: LazyLock<PawnAttackTable> = LazyLock::new(|| {
    [
        init_pseudo_attacks(&[NE, NW]), // White pawn attacks (index 0)
        init_pseudo_attacks(&[SE, SW]), // Black pawn attacks (index 1)
    ]
});

/// Precomputed knight attacks, indexed by `[Square]`.
static KNIGHT_ATTACKS: LazyLock<AttackTable> =
    LazyLock::new(|| init_pseudo_attacks(&[NNE, NNW, NEE, NWW, SEE, SWW, SSE, SSW]));

/// Precomputed king attacks, indexed by `[Square]`.
static KING_ATTACKS: LazyLock<AttackTable> =
    LazyLock::new(|| init_pseudo_attacks(&[N, NE, NW, E, W, SE, SW, S]));

/// Precomputed bishop attacks using magic bitboards.
static BISHOP_TABLE: LazyLock<SliderAttackTable<BISHOP_TABLE_SIZE>> =
    LazyLock::new(|| get_magic_tables::<BISHOP_TABLE_SIZE>(PieceType::Bishop));

/// Precomputed rook attacks using magic bitboards.
static ROOK_TABLE: LazyLock<SliderAttackTable<ROOK_TABLE_SIZE>> =
    LazyLock::new(|| get_magic_tables::<ROOK_TABLE_SIZE>(PieceType::Rook));

/******************************************\
|==========================================|
|              Lookup Tables               |
|==========================================|
\******************************************/

/// Precomputed lines between squares (inclusive), indexed by `[Square][Square]`.
static LINE_BB: LazyLock<SquarePairTable> =
    LazyLock::new(|| init_lookup_table(&mut populate_line_bb));

/// Precomputed lines between squares (exclusive), indexed by `[Square][Square]`.
static BETWEEN_BB: LazyLock<SquarePairTable> =
    LazyLock::new(|| init_lookup_table(&mut populate_between_bb));

/// Precomputed pin masks between squares, indexed by `[Square][Square]`.
static PIN_BB: LazyLock<SquarePairTable> =
    LazyLock::new(|| init_lookup_table(&mut populate_pin_bb));

/// Precomputed check masks between squares, indexed by `[Square][Square]`.
static CHECK_BB: LazyLock<SquarePairTable> =
    LazyLock::new(|| init_lookup_table(&mut populate_check_bb));

/// Precomputed castling rights changes associated with squares, indexed by `[Square]`.
static CASTLING_RIGHTS: LazyLock<CastlingTable> = LazyLock::new(|| init_castling_rights_table());

/// Precomputed Chebyshev distances between squares, indexed by `[Square][Square]`.
static DIST: LazyLock<DistanceTable> = LazyLock::new(|| init_dist_table());

/******************************************\
|==========================================|
|              Initialisation              |
|==========================================|
\******************************************/

/// Forces the initialization of all static lookup tables in this module.
///
/// Calling this function ensures that all precomputed tables (`PAWN_ATTACKS`,
/// `KNIGHT_ATTACKS`, `KING_ATTACKS`, `BISHOP_TABLE`, `ROOK_TABLE`, `LINE_BB`,
/// `BETWEEN_BB`, `PIN_BB`, `CHECK_BB`, `CASTLING_RIGHTS`, `DIST`) are
/// generated and ready for use.
///
/// This is typically called once during engine startup to avoid the small
/// performance overhead of lazy initialization during time-sensitive operations
/// like search.
///
/// # Example
/// ```rust, no_run
/// use sophos::movegen::init_all_tables;
///
/// // Call this early in your program, e.g., in main()
/// init_all_tables();
///
/// // Now all lookup tables are guaranteed to be initialized.
/// ```
pub fn init_all_tables() {
    // Access each LazyLock to force its initialization.
    LazyLock::force(&PAWN_ATTACKS);
    LazyLock::force(&KNIGHT_ATTACKS);
    LazyLock::force(&KING_ATTACKS);
    LazyLock::force(&BISHOP_TABLE);
    LazyLock::force(&ROOK_TABLE);
    LazyLock::force(&LINE_BB);
    LazyLock::force(&BETWEEN_BB);
    LazyLock::force(&PIN_BB);
    LazyLock::force(&CHECK_BB);
    LazyLock::force(&CASTLING_RIGHTS);
    LazyLock::force(&DIST);
}

/// Initializes pseudo-attack tables for non-sliding pieces (Pawn, Knight, King).
/// "Pseudo attacks" are potential moves ignoring blockers.
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

/******************************************\
|==========================================|
|               Get Attacks                |
|==========================================|
\******************************************/

/// Gets the precomputed attack `Bitboard` for a pawn.
///
/// This returns the squares a pawn of the given `Colour` on the given `Square`
/// would attack (i.e., the squares it could capture on). It does not consider
/// whether pieces are present on the target squares.
///
/// # Arguments
/// * `col`: The `Colour` of the pawn.
/// * `sq`: The `Square` the pawn is on.
///
/// # Returns
/// A `Bitboard` representing the squares attacked by the pawn.
///
/// # Example
/// ```rust
/// use sophos::core::{Bitboard, Colour, Square};
/// use sophos::movegen::{init_all_tables, pawn_attack};
///
/// init_all_tables(); // Ensure tables are initialized
///
/// let white_pawn_attacks_e4 = pawn_attack(Colour::White, Square::E4);
/// // White pawn on E4 attacks D5 and F5
/// assert_eq!(white_pawn_attacks_e4, Bitboard::from([Square::D5, Square::F5]));
///
/// let black_pawn_attacks_d5 = pawn_attack(Colour::Black, Square::D5);
/// // Black pawn on D5 attacks C4 and E4
/// assert_eq!(black_pawn_attacks_d5, Bitboard::from([Square::C4, Square::E4]));
/// ```
#[inline]
pub fn pawn_attack(col: Colour, sq: Square) -> Bitboard {
    PAWN_ATTACKS[col as usize][sq as usize]
}

/// Gets the precomputed attack `Bitboard` for a "leaper" piece (Knight or King).
///
/// Leaper pieces jump directly to their destination squares, ignoring any
/// intervening pieces. This function returns the set of squares attacked
/// by the specified piece type from the given square.
///
/// # Arguments
/// * `pt`: The `PieceType` (must be `Knight` or `King`).
/// * `sq`: The `Square` the piece is on.
///
/// # Returns
/// A `Bitboard` representing the squares attacked by the piece.
///
/// # Panics
/// Panics if `pt` is not `PieceType::Knight` or `PieceType::King`.
///
/// # Example
/// ```rust
/// use sophos::core::{Bitboard, PieceType, Square};
/// use sophos::movegen::{init_all_tables, leaper_attack};
///
/// init_all_tables(); // Ensure tables are initialized
///
/// let knight_attacks_g1 = leaper_attack(PieceType::Knight, Square::G1);
/// // Knight on G1 attacks F3 and H3
/// assert!(knight_attacks_g1.get(Square::F3));
/// assert!(knight_attacks_g1.get(Square::H3));
/// assert!(!knight_attacks_g1.get(Square::G3)); // Does not attack G3
///
/// let king_attacks_e1 = leaper_attack(PieceType::King, Square::E1);
/// // King on E1 attacks D1, D2, E2, F2, F1
/// assert!(king_attacks_e1.get(Square::D1));
/// assert!(king_attacks_e1.get(Square::D2));
/// assert!(king_attacks_e1.get(Square::E2));
/// assert!(king_attacks_e1.get(Square::F2));
/// assert!(king_attacks_e1.get(Square::F1));
/// ```
#[inline]
pub fn leaper_attack(pt: PieceType, sq: Square) -> Bitboard {
    match pt {
        PieceType::Knight => KNIGHT_ATTACKS[sq as usize],
        PieceType::King => KING_ATTACKS[sq as usize],
        _ => unreachable!("Only Knight and King are supported by leaper_attack"),
    }
}

/// Gets the attack `Bitboard` for a sliding piece (Bishop, Rook, or Queen).
///
/// This function calculates the squares attacked by a sliding piece from a
/// given square, considering blocking pieces represented by the occupancy bitboard.
/// It uses precomputed magic bitboard tables for efficiency.
///
/// # Arguments
/// * `pt`: The `PieceType` (must be `Bishop`, `Rook`, or `Queen`).
/// * `sq`: The `Square` the sliding piece is on.
/// * `occ`: A `Bitboard` representing all occupied squares on the board (including
///   pieces of both colours). The attacking piece itself should typically *not* be
///   included in `occ` for attack generation, but blockers are.
///
/// # Returns
/// A `Bitboard` representing the squares attacked by the slider, blocked by `occ`.
///
/// # Panics
/// Panics if `pt` is not `PieceType::Bishop`, `PieceType::Rook`, or `PieceType::Queen`.
///
/// # Example
/// ```rust
/// use sophos::core::{Bitboard, PieceType, Square};
/// use sophos::movegen::{init_all_tables, slider_attack};
///
/// init_all_tables(); // Ensure tables are initialized
///
/// // Rook on A1, blocker on A4
/// let occupancy = Bitboard::from_square(Square::A4);
/// let rook_attacks = slider_attack(PieceType::Rook, Square::A1, occupancy);
/// // Attacks A2, A3, and captures A4. Does not attack beyond A4.
/// assert!(rook_attacks.get(Square::A2));
/// assert!(rook_attacks.get(Square::A3));
/// assert!(rook_attacks.get(Square::A4)); // Can capture blocker
/// assert!(!rook_attacks.get(Square::A5)); // Blocked
/// assert!(!rook_attacks.get(Square::B1)); // Only attacks along rank/file
///
/// // Queen on D4, empty board
/// let queen_attacks = slider_attack(PieceType::Queen, Square::D4, Bitboard::EMPTY);
/// // Queen attacks like a rook and bishop combined
/// assert!(queen_attacks.get(Square::D8)); // Rank
/// assert!(queen_attacks.get(Square::H4)); // File
/// assert!(queen_attacks.get(Square::A7)); // Diagonal
/// assert!(queen_attacks.get(Square::G1)); // Diagonal
/// ```
#[inline]
pub fn slider_attack(pt: PieceType, sq: Square, occ: Bitboard) -> Bitboard {
    // Magic bitboard lookups are designed to be safe.
    match pt {
        PieceType::Bishop => BISHOP_TABLE.get_entry(sq, occ),
        PieceType::Rook => ROOK_TABLE.get_entry(sq, occ),
        PieceType::Queen => BISHOP_TABLE.get_entry(sq, occ) | ROOK_TABLE.get_entry(sq, occ),
        _ => unreachable!("Only Bishop, Rook, and Queen are supported by slider_attack"),
    }
}

/******************************************\
|==========================================|
|           Get Bitboard Lookups           |
|==========================================|
\******************************************/

/// Gets the `Bitboard` representing the line connecting two squares, inclusive.
///
/// If the two squares do not lie on the same rank, file, or diagonal, an
/// empty `Bitboard` is returned. Otherwise, returns a bitboard with bits set
/// for all squares on the line segment connecting `from` and `to`, including
/// the `from` and `to` squares themselves.
///
/// # Arguments
/// * `from`: The starting `Square` of the line.
/// * `to`: The ending `Square` of the line.
///
/// # Returns
/// A `Bitboard` representing the line between `from` and `to` (inclusive).
///
/// # Example
/// ```rust
/// use sophos::core::{Bitboard, Square};
/// use sophos::movegen::{init_all_tables, line_bb};
///
/// init_all_tables();
///
/// let line = line_bb(Square::A1, Square::A4);
/// // Includes A1, A2, A3, A4
/// assert_eq!(line, Bitboard::from([Square::A1, Square::A2, Square::A3, Square::A4]));
///
/// let diag_line = line_bb(Square::H1, Square::F3);
/// // Includes H1, G2, F3
/// assert_eq!(diag_line, Bitboard::from([Square::H1, Square::G2, Square::F3]));
///
/// let no_line = line_bb(Square::A1, Square::B3); // Not on same line
/// assert_eq!(no_line, Bitboard::EMPTY);
/// ```
#[inline]
pub fn line_bb(from: Square, to: Square) -> Bitboard {
    LINE_BB[from as usize][to as usize]
}

/// Gets the `Bitboard` representing the squares strictly between two squares.
///
/// If the two squares do not lie on the same rank, file, or diagonal, or if
/// they are adjacent, an empty `Bitboard` is returned. Otherwise, returns a
/// bitboard with bits set for all squares on the line segment connecting
/// `from` and `to`, *excluding* the `from` and `to` squares themselves.
///
/// # Arguments
/// * `from`: The starting `Square`.
/// * `to`: The ending `Square`.
///
/// # Returns
/// A `Bitboard` representing the squares between `from` and `to` (exclusive).
///
/// # Example
/// ```rust
/// use sophos::core::{Bitboard, Square};
/// use sophos::movegen::{init_all_tables, between_bb};
///
/// init_all_tables();
///
/// let between = between_bb(Square::A1, Square::A4);
/// // Includes A2, A3 (excludes A1, A4)
/// assert_eq!(between, Bitboard::from([Square::A2, Square::A3]));
///
/// let diag_between = between_bb(Square::H1, Square::F3);
/// // Includes G2 (excludes H1, F3)
/// assert_eq!(diag_between, Bitboard::from_square(Square::G2));
///
/// let adjacent = between_bb(Square::A1, Square::A2); // Adjacent
/// assert_eq!(adjacent, Bitboard::EMPTY);
///
/// let no_line = between_bb(Square::A1, Square::B3); // Not on same line
/// assert_eq!(no_line, Bitboard::EMPTY);
/// ```
#[inline]
pub fn between_bb(from: Square, to: Square) -> Bitboard {
    BETWEEN_BB[from as usize][to as usize]
}

/// Gets the `Bitboard` representing a potential pin ray or line of movement for a pinned piece.
///
/// This returns the squares between the `pinner` square and the `pinned_or_king` square,
/// *plus* the `pinner` square itself. This mask represents the squares a piece located
/// on the line between `pinner` and `pinned_or_king` could potentially move to if it were pinned
/// to its king (located at `pinned_or_king`) by a piece on `pinner`.
///
/// If `pinner` and `pinned_or_king` are not on the same rank, file, or diagonal,
/// returns an empty `Bitboard`.
///
/// # Arguments
/// * `pinner`: The `Square` of the attacking (pinning) piece.
/// * `king`: The `Square` of the king the piece is being pinned to, or the king itself.
///
/// # Returns
/// A `Bitboard` mask for validating moves of a potentially pinned piece.
///
/// # Example
/// ```rust
/// use sophos::core::{Bitboard, Square};
/// use sophos::movegen::{init_all_tables, pin_bb};
///
/// init_all_tables();
///
/// // Bishop on A1 pins a Knight on C3 to the King on E5
/// let pin_mask = pin_bb(Square::A1, Square::E5);
/// // The pinned Knight on C3 can only move to B2 or D4 along the pin line.
/// // The mask includes the pinner (A1) and the squares between (B2, C3, D4).
/// assert_eq!(pin_mask, Bitboard::from([Square::A1, Square::B2, Square::C3, Square::D4]));
/// assert!(pin_mask.get(Square::B2));
/// assert!(pin_mask.get(Square::D4));
/// assert!(!pin_mask.get(Square::E5)); // King's square is not included
/// ```
#[inline]
pub fn pin_bb(pinner: Square, king: Square) -> Bitboard {
    PIN_BB[pinner as usize][king as usize]
}

/// Gets the `Bitboard` representing squares that can resolve a check from a sliding piece.
///
/// When a king is checked by a sliding piece (Bishop, Rook, or Queen), the check
/// can be resolved either by capturing the attacker or by blocking the attack
/// by placing a piece on a square between the attacker and the king.
///
/// This function returns a bitboard containing the attacker's square (`checker`)
/// and all squares strictly between the `checker` and the `king`. Moving a piece
/// to any of these squares resolves the check.
///
/// If `checker` and `king` are not on the same rank, file, or diagonal (e.g., for
/// a Knight check), the behavior might yield an empty or irrelevant bitboard, as
/// this function is primarily designed for slider checks.
///
/// # Arguments
/// * `checker`: The `Square` of the attacking (checking) sliding piece.
/// * `king`: The `Square` of the king being checked.
///
/// # Returns
/// A `Bitboard` mask of squares to capture the checker or block the check.
///
/// # Example
/// ```rust
/// use sophos::core::{Bitboard, Square};
/// use sophos::movegen::{init_all_tables, check_bb};
///
/// init_all_tables();
///
/// // Rook on A8 checks King on A1
/// let check_mask = check_bb(Square::A8, Square::A1);
/// // Can resolve by capturing on A8 or blocking on A2, A3, A4, A5, A6, A7
/// let expected = Bitboard::from([
///     Square::A8, Square::A7, Square::A6, Square::A5, Square::A4, Square::A3, Square::A2
/// ]);
/// assert_eq!(check_mask, expected);
/// ```
#[inline]
pub fn check_bb(checker: Square, king: Square) -> Bitboard {
    CHECK_BB[checker as usize][king as usize]
}

/// Gets the `Castling` rights that are removed if a piece moves *to* or *from* the given square.
///
/// Certain moves invalidate castling rights. Specifically:
/// - If the White King moves from/to E1, White loses both Kingside and Queenside rights.
/// - If the Black King moves from/to E8, Black loses both Kingside and Queenside rights.
/// - If a Rook moves from/to A1, White loses Queenside rights.
/// - If a Rook moves from/to H1, White loses Kingside rights.
/// - If a Rook moves from/to A8, Black loses Queenside rights.
/// - If a Rook moves from/to H8, Black loses Kingside rights.
///
/// This function returns a `Castling` bitmask representing the rights that *must be removed*
/// based on the specified square being the origin or destination of a move. To update
/// the board's castling rights, you typically use `current_rights &= !castling_rights(square);`.
///
/// # Arguments
/// * `sq`: The `Square` involved in the move (either origin or destination).
///
/// # Returns
/// A `Castling` bitmask indicating the rights to be removed.
///
/// # Example
/// ```rust
/// use sophos::core::{Castling, Square};
/// use sophos::movegen::{init_all_tables, castling_rights};
///
/// init_all_tables();
///
/// let mut current_rights = Castling::ALL;
///
/// // White moves King from E1
/// current_rights &= !castling_rights(Square::E1);
/// assert!(!current_rights.has(Castling::WK));
/// assert!(!current_rights.has(Castling::WQ));
/// assert!(current_rights.has(Castling::BK)); // Black rights unaffected
///
/// // Black moves Rook from A8
/// current_rights &= !castling_rights(Square::A8);
/// assert!(!current_rights.has(Castling::BQ));
/// assert!(current_rights.has(Castling::BK)); // Black Kingside unaffected
///
/// // Moving a piece from/to D4 doesn't affect castling
/// let rights_to_remove = castling_rights(Square::D4);
/// assert_eq!(rights_to_remove, Castling::NONE);
/// ```
#[inline]
pub fn castling_rights(sq: Square) -> Castling {
    // SAFETY: Table initialized, index valid.
    CASTLING_RIGHTS[sq as usize]
}

/// Gets the Chebyshev distance (also known as maximum norm or king distance) between two squares.
///
/// This distance is calculated as `max(abs(rank1 - rank2), abs(file1 - file2))`.
/// It represents the minimum number of King moves required to travel between the two squares.
///
/// # Arguments
/// * `sq1`: The first `Square`.
/// * `sq2`: The second `Square`.
///
/// # Returns
/// The `u8` Chebyshev distance between `sq1` and `sq2`.
///
/// # Example
/// ```rust
/// use sophos::core::Square;
/// use sophos::movegen::{init_all_tables, dist};
///
/// init_all_tables();
///
/// assert_eq!(dist(Square::A1, Square::A1), 0);
/// assert_eq!(dist(Square::A1, Square::H8), 7); // max(abs(0-7), abs(0-7)) = 7
/// assert_eq!(dist(Square::E4, Square::C6), 2); // max(abs(3-5), abs(4-2)) = max(2, 2) = 2
/// assert_eq!(dist(Square::H1, Square::A2), 7); // max(abs(0-1), abs(7-0)) = max(1, 7) = 7
/// ```
#[inline]
pub fn dist(sq1: Square, sq2: Square) -> u8 {
    // SAFETY: Table initialized, indices valid.
    DIST[sq1 as usize][sq2 as usize]
}

// Tests remain unchanged, but ensure they call init_all_tables() first if needed.
#[cfg(test)]
mod test {
    use super::*;
    // Assuming attacks_on_the_fly is pub(crate) or pub(super) in magic.rs
    use crate::movegen::magic::attacks_on_the_fly;
    use crate::utils::PRNG;

    #[test]
    fn test_init_all_attack_tables() {
        init_all_tables();
        // The function call itself is the test here. No panic means success.
    }

    #[test]
    fn test_pawn_attacks() {
        for sq in Square::iter() {
            let attack = pawn_attack(Colour::White, sq);
            let sq_bb = Bitboard::from(sq);
            let naive_attack =
                Bitboard::shift(&sq_bb, Direction::NE) | Bitboard::shift(&sq_bb, Direction::NW);
            assert_eq!(
                attack, naive_attack,
                "White pawn attack mismatch for {:?}",
                sq
            );

            let attack = pawn_attack(Colour::Black, sq);
            let sq_bb = Bitboard::from(sq);
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
            assert_eq!(attack, naive_attack, "Knight attack mismatch for {:?}", sq);
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
            assert_eq!(attack, naive_attack, "King attack mismatch for {:?}", sq);
        }
    }

    #[test]
    fn test_bishop_attacks() {
        let mut rng = PRNG::default();

        for _ in 0..1000 {
            // Reduced iterations for faster tests
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq); // Attacker doesn't block itself
                let attack = slider_attack(PieceType::Bishop, sq, occ);
                let naive_attack = attacks_on_the_fly(PieceType::Bishop, sq, occ);
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
            // Reduced iterations
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq);
                let attack = slider_attack(PieceType::Rook, sq, occ);
                let naive_attack = attacks_on_the_fly(PieceType::Rook, sq, occ);
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
            // Reduced iterations
            let mut occ = Bitboard(rng.random_u64());

            for sq in Square::iter() {
                occ.clear(sq);
                let attack = slider_attack(PieceType::Queen, sq, occ);
                let naive_attack = attacks_on_the_fly(PieceType::Bishop, sq, occ)
                    | attacks_on_the_fly(PieceType::Rook, sq, occ);
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

                if from != to && !bb.is_empty() {
                    assert!(
                        bb.get(from),
                        "Line missing 'from' square {:?} for {:?}-{:?}",
                        from,
                        from,
                        to
                    );
                    assert!(
                        bb.get(to),
                        "Line missing 'to' square {:?} for {:?}-{:?}",
                        to,
                        from,
                        to
                    );
                } else if from == to {
                    assert_eq!(
                        bb,
                        Bitboard::EMPTY,
                        "Line for same square failed {:?}",
                        from
                    );
                }
            }
        }
        // Specific cases
        assert_eq!(
            line_bb(Square::A1, Square::A4),
            Bitboard::from([
                Square::A1,
                Square::A2,
                Square::A3,
                Square::A4,
                Square::A5,
                Square::A6,
                Square::A7,
                Square::A8
            ])
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
                Square::A8
            ])
        );
        assert_eq!(line_bb(Square::A1, Square::B3), Bitboard::EMPTY);
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

                // Between should never contain the endpoints
                assert!(
                    !bb.get(from),
                    "Between contains 'from' square {:?} for {:?}-{:?}",
                    from,
                    from,
                    to
                );
                assert!(
                    !bb.get(to),
                    "Between contains 'to' square {:?} for {:?}-{:?}",
                    to,
                    from,
                    to
                );
            }
        }
        // Specific cases
        assert_eq!(
            between_bb(Square::A1, Square::A4),
            Bitboard::from([Square::A2, Square::A3])
        );
        assert_eq!(
            between_bb(Square::H1, Square::F3),
            Bitboard::from(Square::G2)
        );
        assert_eq!(between_bb(Square::A1, Square::A2), Bitboard::EMPTY);
        assert_eq!(between_bb(Square::A1, Square::B3), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_bb() {
        for pinner in Square::iter() {
            for pinned_or_king in Square::iter() {
                let bb = pin_bb(pinner, pinned_or_king);

                // Pin mask includes pinner, excludes pinned/king square (unless adjacent)
                if !bb.is_empty() {
                    assert!(
                        bb.get(pinner),
                        "Pin mask missing pinner {:?} for {:?}-{:?}",
                        pinner,
                        pinner,
                        pinned_or_king
                    );
                    if dist(pinner, pinned_or_king) > 1 {
                        assert!(
                            !bb.get(pinned_or_king),
                            "Pin mask includes pinned/king {:?} for {:?}-{:?}",
                            pinned_or_king,
                            pinner,
                            pinned_or_king
                        );
                    }
                }
                // Check relationship with between_bb
                assert_eq!(
                    bb & !Bitboard::from(pinner),
                    between_bb(pinner, pinned_or_king),
                    "Pin/Between relationship failed for {:?}-{:?}",
                    pinner,
                    pinned_or_king
                );
            }
        }
        // Specific case from example
        assert_eq!(
            pin_bb(Square::A1, Square::E5),
            Bitboard::from([Square::A1, Square::B2, Square::C3, Square::D4])
        );
    }

    #[test]
    fn test_check_bb() {
        for checker in Square::iter() {
            for king in Square::iter() {
                let bb = check_bb(checker, king);

                // Check mask includes checker, excludes king (unless adjacent)
                if !bb.is_empty() {
                    assert!(
                        bb.get(checker),
                        "Check mask missing checker {:?} for {:?}-{:?}",
                        checker,
                        checker,
                        king
                    );
                    if dist(checker, king) > 1 {
                        assert!(
                            !bb.get(king),
                            "Check mask includes king {:?} for {:?}-{:?}",
                            king,
                            checker,
                            king
                        );
                    }
                }
                // Check relationship with between_bb
                assert_eq!(
                    bb & !Bitboard::from(checker),
                    between_bb(checker, king),
                    "Check/Between relationship failed for {:?}-{:?}",
                    checker,
                    king
                );
            }
        }
        // Specific case from example
        let expected = Bitboard::from([
            Square::A8,
            Square::A7,
            Square::A6,
            Square::A5,
            Square::A4,
            Square::A3,
            Square::A2,
        ]);
        assert_eq!(check_bb(Square::A8, Square::A1), expected);
    }

    #[test]
    fn test_castling_rights_table() {
        assert_eq!(castling_rights(Square::E1), !Castling::WHITE_CASTLING);
        assert_eq!(castling_rights(Square::E8), !Castling::BLACK_CASTLING);
        assert_eq!(castling_rights(Square::A1), !Castling::WQ);
        assert_eq!(castling_rights(Square::H1), !Castling::WK);
        assert_eq!(castling_rights(Square::A8), !Castling::BQ);
        assert_eq!(castling_rights(Square::H8), !Castling::BK);
        // Test a square that doesn't affect rights
        assert_eq!(castling_rights(Square::D4), !Castling::NONE);
        assert_eq!(castling_rights(Square::G5), !Castling::NONE);
    }

    #[test]
    fn test_sq_dist() {
        assert_eq!(dist(Square::A1, Square::A6), 5);
        assert_eq!(dist(Square::E5, Square::F6), 1);
        assert_eq!(dist(Square::H1, Square::A8), 7);
        assert_eq!(dist(Square::C3, Square::C3), 0);
    }
}
