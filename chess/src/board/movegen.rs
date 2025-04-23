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

mod r#gen;
mod init;
mod lookup;
mod magic;
mod move_list;

pub(crate) use lookup::{leaper_attack, pawn_attack, pawn_attack_span, pin_bb, slider_attack};

pub use move_list::MoveList;

pub(crate) use r#gen::generate_move;

#[derive(PartialEq, Eq)]
pub enum MoveGenType {
    Legal,
    Quiet,
    Capture,
}

pub trait GenTypeTrait {
    fn gen_type() -> MoveGenType;
}

pub struct LegalGen;
pub struct QuietGen;
pub struct CaptureGen;

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

use super::Board;
use crate::core::*;

impl Board {
    #[inline]
    pub fn generate_moves<G: GenTypeTrait>(&self, move_list: &mut MoveList) {
        generate_move::<G>(self, move_list);
    }

    #[inline]
    pub(crate) fn castling_king_dest(&self, castle: Castling) -> Square {
        debug_assert!(
            castle.0.count_ones() == 1,
            "This function only works for castling on one side (atomic)"
        );

        match castle {
            Castling::WK => Square::G1,
            Castling::WQ => Square::C1,
            Castling::BK => Square::G8,
            Castling::BQ => Square::C8,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub(crate) fn castling_rook_dest(&self, castle: Castling) -> Square {
        debug_assert!(
            castle.0.count_ones() == 1,
            "This function only works for castling on one side (atomic)"
        );

        match castle {
            Castling::WK => Square::F1,
            Castling::WQ => Square::D1,
            Castling::BK => Square::F8,
            Castling::BQ => Square::D8,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub(crate) fn castling_flag(&self, castle: Castling) -> MoveFlag {
        debug_assert!(
            castle.0.count_ones() == 1,
            "This function only works for castling on one side (atomic)"
        );

        match castle {
            Castling::WK | Castling::BK => MoveFlag::KingCastle,
            Castling::WQ | Castling::BQ => MoveFlag::QueenCastle,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub(crate) fn can_castle(&self, castle: Castling) -> bool {
        debug_assert!(
            castle.0.count_ones() == 1,
            "This function only works for castling on one side (atomic)"
        );

        let us = self.side_to_move();

        let ksq = self.ksq(us);
        // Safety: rook_sq lookup is safe if castling rights are present initially.
        let rook_sq = unsafe { self.rook_sq(castle.0.trailing_zeros() as usize) };
        // Rook destination (depends on castling rights)
        let rook_dest = self.castling_rook_dest(castle);

        // Calculate movement paths using pin_bb for convenience (line between squares + target square)
        // King path: squares the king traverses (e.g., E1->G1 includes F1, G1)
        let king_path = pin_bb(ksq, self.castling_king_dest(castle));
        // Rook path: squares the rook traverses (e.g., H1->F1 includes G1, F1)
        let rook_path = pin_bb(rook_sq, rook_dest);
        // Combined area that must be empty (excluding king and rook) and king path squares cannot be attacked.
        let move_area = king_path | rook_path;

        // Occupancy excluding the king and the specific castling rook
        let occ = self.all_occupied_bb() ^ rook_sq.bb() ^ ksq.bb();

        (king_path & self.attacked()).is_empty() && (move_area & occ).is_empty()
    }
}
