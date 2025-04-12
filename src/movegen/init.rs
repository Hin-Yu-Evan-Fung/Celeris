//! # Module: `init`
//!
//! This module provides functions for initializing various lookup tables used in move generation.
//! These tables are precomputed to optimize the performance of the chess engine.
//!
//! ## Overview
//!
//! The module contains functions to initialize the following lookup tables:
//!
//! - **Line Bitboard Table (`LINE_BB`)**: Stores bitboards representing the squares on a line between two given squares, inclusive.
//! - **Between Bitboard Table (`BETWEEN_BB`)**: Stores bitboards representing the squares strictly between two given squares, exclusive.
//! - **Pin Bitboard Table (`PIN_BB`)**: Stores bitboards representing potential pin rays or lines of movement for pinned pieces.
//! - **Check Bitboard Table (`CHECK_BB`)**: Stores bitboards representing squares that can resolve a check from a sliding piece.
//! - **Distance Table (`DIST`)**: Stores the Chebyshev distance between any two squares on the board.
//! - **Castling Rights Table (`CASTLING_RIGHTS`)**: Stores the castling rights that are removed if a piece moves to or from a given square.
//!
//! ## Functions
//!
//! - `populate_line_bb`: Populates the `LINE_BB` table for a given pair of squares.
//! - `populate_between_bb`: Populates the `BETWEEN_BB` table for a given pair of squares.
//! - `populate_pin_bb`: Populates the `PIN_BB` table for a given pair of squares.
//! - `populate_check_bb`: Populates the `CHECK_BB` table for a given pair of squares.
//! - `init_lookup_table`: A generic helper function to initialize square-pair lookup tables.
//! - `init_dist_table`: Initializes the `DIST` table.
//! - `init_castling_rights_table`: Initializes the `CASTLING_RIGHTS` table.
//!
//! ## Usage
//!
//! These functions are typically called during the initialization of the chess engine to precompute
//! the lookup tables. The tables are then used by other modules, such as `lookup`, to efficiently
//! generate moves and evaluate board positions.
use super::lookup::*;
use crate::core::*;

// Populates the LINE_BB table.
pub(super) fn populate_line_bb(
    table: &mut SquarePairTable,
    pt: PieceType,
    from: Square,
    to: Square,
) {
    let from_bb: Bitboard = from.into();
    let to_bb: Bitboard = to.into();
    // Use the public slider_attack function which relies on the (potentially lazy) tables
    let from_ray = slider_attack(pt, from, Bitboard::EMPTY);
    let to_ray = slider_attack(pt, to, Bitboard::EMPTY);
    // Line mask is the intersection of the two rays, plus the squares themselves.
    // If the squares are not on the same line, the result is an empty bitboard.
    // If the squares are the same, the result is an empty bitboard.
    table[from as usize][to as usize] = (from_ray & to_ray) | (from_bb | to_bb);
}

// Populates the BETWEEN_BB table.
pub(super) fn populate_between_bb(
    table: &mut SquarePairTable,
    pt: PieceType,
    from: Square,
    to: Square,
) {
    // Use the public slider_attack function
    let from_ray = slider_attack(pt, from, to.into());
    let to_ray = slider_attack(pt, to, from.into());
    // Between mask is the line between the squares, excluding the squares themselves.
    // If the squares are not on the same line, the result is an empty bitboard.
    // If the squares are adjacent, the result is an empty bitboard.
    // If the squares are the same, the result is an empty bitboard.
    table[from as usize][to as usize] = from_ray & to_ray;
}

// Populates the PIN_BB table.
pub(super) fn populate_pin_bb(
    table: &mut SquarePairTable,
    pt: PieceType,
    from: Square,
    to: Square,
) {
    // Use the public slider_attack function
    let from_ray = slider_attack(pt, from, to.into());
    let to_ray = slider_attack(pt, to, from.into());
    // Pin mask includes the line *between* the pinner and pinned piece, plus the pinner itself.
    // Assuming 'from' is the pinner and 'to' is the pinned piece's square (or king).
    // The resulting mask represents squares the pinned piece *could* move to along the pin line.
    table[from as usize][to as usize] = (from_ray & to_ray) | Bitboard::from(from); // Include the 'from' square (pinner)
}

// Populates the CHECK_BB table.
pub(super) fn populate_check_bb(
    table: &mut SquarePairTable,
    pt: PieceType,
    from: Square,
    to: Square,
) {
    // Use the public slider_attack function
    let from_ray = slider_attack(pt, from, to.into()); // Ray from attacker towards king
    let to_ray = slider_attack(pt, to, from.into()); // Ray from king towards attacker
    let between = from_ray & to_ray; // Squares between attacker and king

    // Check mask includes squares between attacker and king, plus the attacker's square.
    // This represents squares where a piece can block the check or capture the attacker.
    // Assuming 'from' is the attacker and 'to' is the king.
    table[from as usize][to as usize] = between | Bitboard::from(from); // Include the 'from' square (attacker)
}

/// Generic helper to initialize square-pair lookup tables.
pub(super) fn init_lookup_table(populate_fn: &mut PopulateTableFn) -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    for pt in [PieceType::Bishop, PieceType::Rook] {
        for from in Square::iter() {
            // Iterate over all squares attacked by the slider from 'from' on an empty board
            slider_attack(pt, from, Bitboard::EMPTY).for_each(|to| {
                // Populate the table entry for the pair (from, to)
                populate_fn(&mut table, pt, from, to);
            });
        }
    }

    table
}

/// Initializes the Chebyshev distance table.
pub(super) fn init_dist_table() -> [[u8; Square::NUM]; Square::NUM] {
    let mut table = [[0u8; Square::NUM]; Square::NUM];

    for sq1 in Square::iter() {
        for sq2 in Square::iter() {
            table[sq1 as usize][sq2 as usize] =
                std::cmp::max(Square::rank_dist(sq1, sq2), Square::file_dist(sq1, sq2));
        }
    }

    table
}

/// Initializes the castling rights update mask table.
/// The value `table[sq]` is a mask that should be ANDed with the
/// board's current castling rights when a piece moves *to* or *from* `sq`.
/// It effectively removes the rights associated with that square's role (king or rook start square).
pub(super) fn init_castling_rights_table() -> [Castling; Square::NUM] {
    use Square::*;

    // Start with a mask that keeps all rights.
    let mut table = [Castling::ALL; Square::NUM];

    // For specific squares, modify the mask to *remove* the relevant right(s).
    // Moving the E1 King removes both WK and WQ.
    table[E1 as usize].remove(Castling::WHITE_CASTLING);
    // Moving the E8 King removes both BK and BQ.
    table[E8 as usize].remove(Castling::BLACK_CASTLING);
    // Moving the A1 Rook removes WQ.
    table[A1 as usize].remove(Castling::WQ);
    // Moving the H1 Rook removes WK.
    table[H1 as usize].remove(Castling::WK);
    // Moving the A8 Rook removes BQ.
    table[A8 as usize].remove(Castling::BQ);
    // Moving the H8 Rook removes BK.
    table[H8 as usize].remove(Castling::BK);

    table
}
