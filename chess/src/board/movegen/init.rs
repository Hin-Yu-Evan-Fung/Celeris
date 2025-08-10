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
//!
//! ## Functions
//!
//! - `populate_line_bb`: Populates the `LINE_BB` table for a given pair of squares.
//! - `populate_between_bb`: Populates the `BETWEEN_BB` table for a given pair of squares.
//! - `populate_pin_bb`: Populates the `PIN_BB` table for a given pair of squares.
//! - `populate_check_bb`: Populates the `CHECK_BB` table for a given pair of squares.
//! - `init_lookup_table`: A generic helper function to initialize square-pair lookup tables.
//! - `init_dist_table`: Initializes the `DIST` table.
//!
//! ## Usage
//!
//! These functions are typically called during the initialization of the chess engine to precompute
//! the lookup tables. The tables are then used by other modules, such as `lookup`, to efficiently
//! generate moves and evaluate board positions.
use super::lookup::*;
use crate::core::*;

// Returns the line crossing 2 squares
const fn line_bb(pt: PieceType, from: Square, to: Square) -> Bitboard {
    let from_bb: Bitboard = from.bb();
    let to_bb: Bitboard = to.bb();
    // Use the public attacks_on_the_fly function which relies on the (potentially lazy) tables

    let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), Bitboard::EMPTY);

    Bitboard((from_ray.0 & to_ray.0) | from_bb.0 | to_bb.0)
}

// Populate line bb table for Diagonal or Vertical lines (Depending on Piece Type)
const fn populate_line_bb(table: &mut SquarePairTable, pt: PieceType, from: Square) {
    let mut bb = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    while !bb.is_empty() {
        let to = bb.pop_lsb_unchecked();
        // Populate the table entry for the pair (from, to)
        table[from.index()][to.index()] = line_bb(pt, from, to);
    }
}

/// Initialise line bb table
pub(super) const fn init_line_bb_table() -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let from = Square::from_unchecked(i as u8);

        populate_line_bb(&mut table, PieceType::Bishop, from);
        populate_line_bb(&mut table, PieceType::Rook, from);

        i += 1;
    }

    table
}

// Returns the line segment between 2 squares
// Between mask is the line between the squares, excluding the squares themselves.
// If the squares are not on the same line, the result is an empty bitboard.
// If the squares are adjacent, the result is an empty bitboard.
// If the squares are the same, the result is an empty bitboard.
const fn between_bb(pt: PieceType, from: Square, to: Square) -> Bitboard {
    let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), to.bb());
    let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), from.bb());

    Bitboard(from_ray.0 & to_ray.0)
}

// Populate between bb table for Diagonal or Vertical lines (Depending on Piece Type)
const fn populate_between_bb(table: &mut SquarePairTable, pt: PieceType, from: Square) {
    let mut bb = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    while !bb.is_empty() {
        let to = bb.pop_lsb_unchecked();
        // Populate the table entry for the pair (from, to)
        table[from.index()][to.index()] = between_bb(pt, from, to);
    }
}

/// Initialise between bb table
pub(super) const fn init_between_bb_table() -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let from = Square::from_unchecked(i as u8);

        populate_between_bb(&mut table, PieceType::Bishop, from);
        populate_between_bb(&mut table, PieceType::Rook, from);

        i += 1;
    }

    table
}

// Returns pin bb between 2 squares
// Pin mask includes the line *between* the pinner and pinned piece, plus the pinner itself.
// Assuming 'from' is the pinner and 'to' is the pinned piece's square (or king).
// The resulting mask represents squares the pinned piece *could* move to along the pin line.
const fn pin_bb(pt: PieceType, from: Square, to: Square) -> Bitboard {
    // Use the public attacks_on_the_fly function
    let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), to.bb());
    let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), from.bb());

    Bitboard(from_ray.0 & to_ray.0 | to.bb().0) // Include the 'to' square (pinner)
}

// Populate pin bb table for Diagonal or Vertical lines (Depending on Piece Type)
const fn populate_pin_bb(table: &mut SquarePairTable, pt: PieceType, from: Square) {
    let mut bb = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    while !bb.is_empty() {
        let to = bb.pop_lsb_unchecked();
        // Populate the table entry for the pair (from, to)
        table[from.index()][to.index()] = pin_bb(pt, from, to);
    }
}

/// Initialise pin bb table
pub(super) const fn init_pin_bb_table() -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let from = Square::from_unchecked(i as u8);

        populate_pin_bb(&mut table, PieceType::Bishop, from);
        populate_pin_bb(&mut table, PieceType::Rook, from);

        i += 1;
    }

    table
}

// Returns check bb between 2 squares
// Check mask includes squares between attacker and king, plus the attacker's square.
// This represents squares where a piece can block the check or capture the attacker.
// Assuming 'from' is the attacker and 'to' is the king.
const fn check_bb(pt: PieceType, from: Square, to: Square) -> Bitboard {
    // Use the public attacks_on_the_fly function
    let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), to.bb());
    let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), from.bb());

    let dir = match Direction::try_from(to, from) {
        Ok(dir) => dir,
        Err(_) => unreachable!(),
    };

    let bb = match from.add(dir) {
        Ok(sq) => sq.bb(),
        Err(_) => Bitboard::EMPTY,
    };

    Bitboard(from_ray.0 & to_ray.0 | from.bb().0 | bb.0) // Include the 'from' square (checker)
}

// Populate check bb table for Diagonal or Vertical lines (Depending on Piece Type)
const fn populate_check_bb(table: &mut SquarePairTable, pt: PieceType, from: Square) {
    let mut bb = Bitboard::attack_on_the_fly(pt, from.bb(), Bitboard::EMPTY);
    while !bb.is_empty() {
        let to = bb.pop_lsb_unchecked();
        // Populate the table entry for the pair (from, to)
        table[from.index()][to.index()] = check_bb(pt, from, to);
    }
}

/// Initialise check bb table
pub(super) const fn init_check_bb_table() -> SquarePairTable {
    let mut table = [[Bitboard::EMPTY; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let from = Square::from_unchecked(i as u8);

        populate_check_bb(&mut table, PieceType::Bishop, from);
        populate_check_bb(&mut table, PieceType::Rook, from);

        i += 1;
    }

    table
}

// // Populates the CHECK_BB table.
// pub(super) fn populate_check_bb(
//     table: &mut SquarePairTable,
//     pt: PieceType,
//     from: Square,
//     to: Square,
// ) {
//     // Use the public attacks_on_the_fly function
//     let from_ray = Bitboard::attack_on_the_fly(pt, from.bb(), to.bb()); // Ray from attacker towards king
//     let to_ray = Bitboard::attack_on_the_fly(pt, to.bb(), from.bb()); // Ray from king towards attacker
//     let between = from_ray & to_ray; // Squares between attacker and king

//     table[from.index()][to.index()] = between | from.bb(); // Include the 'from' square (attacker)
// }

/// Initializes the Chebyshev distance table.
pub(super) const fn init_dist_table() -> [[u8; Square::NUM]; Square::NUM] {
    let mut table = [[0u8; Square::NUM]; Square::NUM];

    let mut i = 0;
    while i < Square::NUM {
        let mut j = 0;
        while j < Square::NUM {
            let sq1 = Square::from_unchecked(i as u8);
            let sq2 = Square::from_unchecked(j as u8);

            let rank_dist = Square::rank_dist(sq1, sq2);
            let file_dist = Square::file_dist(sq1, sq2);

            if rank_dist > file_dist {
                table[i][j] = rank_dist;
            } else {
                table[i][j] = file_dist;
            }

            j += 1;
        }

        i += 1;
    }

    table
}

/// Initializes pseudo-attack tables for non-sliding pieces (Pawn, Knight, King).
/// "Pseudo attacks" are potential moves ignoring blockers.
pub(super) const fn init_pseudo_attacks(dirs: &[Direction]) -> AttackTable {
    let mut attacks = [Bitboard::EMPTY; Square::NUM];

    let mut i = 0;

    while i < Square::NUM {
        let sq_bb = Square::from_unchecked(i as u8).bb();

        let mut j = 0;
        while j < dirs.len() {
            attacks[i] = Bitboard(attacks[i].0 | Bitboard::shift(&sq_bb, dirs[j]).0);
            j += 1;
        }

        i += 1;
    }

    attacks
}
