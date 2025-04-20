//! # Module: `movegen`
//!
//! This module is responsible for generating legal moves in a chess game. It provides the
//! necessary data structures and algorithms to calculate the possible moves for each piece
//! on the board, taking into account the current board state and the rules of chess.
//!
//! ## Overview
//!
//! The `movegen` module is a critical component of the chess engine, as it forms the basis
//! for all move-related operations. It includes the following key functionalities:
//!
//! - **Attack Generation**: Calculating the squares attacked by each piece type, including
//!   sliding pieces (bishops, rooks, queens) and non-sliding pieces (pawns, knights, kings).
//! - **Move Legality**: Determining whether a move is legal based on the current board state,
//!   including checks, pins, and other constraints.
//! - **Move Encoding**: Representing moves in a compact and efficient format.
//! - **Table Initialization**: Precomputing and storing various lookup tables to speed up
//!   move generation.
//!
//! ## Key Components
//!
//! - **`lookup`**: Contains functions for generating attacks and other move-related information.
//! - **`magic`**: Implements the magic bitboard technique for efficient sliding piece attack
//!   generation.
//! - **`magic_numbers`**: Stores the precomputed magic numbers used in the magic bitboard
//!   algorithm.
//! - **`init`**: Contains functions for initializing the lookup tables.
//!
//! ## Usage
//!
//! The `movegen` module is used by the main chess engine to generate legal moves for a given
//! board state. The `init_all_tables` function must be called once at the start of the
//! program to initialize the lookup tables.
//!
//! ## Implementation Details
//!
//! The module uses bitboards extensively for efficient representation and manipulation of
//! piece positions and attacks. Magic bitboards are used to speed up the calculation of
//! sliding piece attacks.
//!
//! ## Submodules
//!
//! - `init`: Initialization of lookup tables.
//! - `lookup`: Functions for generating attacks and other move-related information.
//! - `magic`: Implementation of magic bitboards.
//! - `magic_numbers`: Precomputed magic numbers.
//!

mod init;
pub mod lookup;
mod magic;
pub mod move_gen;
pub mod move_list;

pub use lookup::{
    between_bb, check_bb, leaper_attack, line_bb, pawn_attack, pawn_attack_span, pin_bb,
    slider_attack,
};

pub use move_list::MoveList;

#[derive(PartialEq, Eq)]
pub enum MoveGenType {
    Legal,
    Quiet,
    Capture,
    Evasions,
}

pub trait GenTypeTrait {
    fn gen_type() -> MoveGenType;
}

pub struct LegalGen;
pub struct QuietGen;
pub struct CaptureGen;
pub struct EvasionsGen;

impl GenTypeTrait for LegalGen {
    fn gen_type() -> MoveGenType {
        MoveGenType::Legal
    }
}
impl GenTypeTrait for QuietGen {
    fn gen_type() -> MoveGenType {
        MoveGenType::Quiet
    }
}
impl GenTypeTrait for CaptureGen {
    fn gen_type() -> MoveGenType {
        MoveGenType::Capture
    }
}
impl GenTypeTrait for EvasionsGen {
    fn gen_type() -> MoveGenType {
        MoveGenType::Evasions
    }
}
