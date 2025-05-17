/******************************************\
|==========================================|
|         Module import and exports        |
|==========================================|
\******************************************/

pub mod r#gen;
pub mod init;
pub mod lookup;
pub mod magic;
pub mod move_list;

pub(crate) use r#gen::generate_move;
pub use lookup::{
    aligned, attacks, bishop_attacks, king_attack, knight_attack, pawn_attack, pin_bb,
    queen_attacks, rook_attacks, sq_dist,
};
pub use magic::init_magic_tables;
pub use move_list::MoveList;

/******************************************\
|==========================================|
|           Move Generation Types          |
|==========================================|
\******************************************/

/// # Move Generation Type representation
///
/// - The current move generator allows generating all legal moves, quiet legal moves or capture legal moves.
#[derive(PartialEq, Eq)]
pub enum MoveGenType {
    Legal,
    Quiet,
    Capture,
}

/******************************************\
|==========================================|
|          Template Implementation         |
|==========================================|
\******************************************/

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

/******************************************\
|==========================================|
|          Board MoveGen Helpers           |
|==========================================|
\******************************************/

use super::Board;
use crate::core::*;

impl Board {
    /// Wrapper for generating moves via the move gen module
    #[inline]
    pub fn generate_moves<G: GenTypeTrait>(&self, move_list: &mut MoveList) {
        generate_move::<G>(self, move_list);
    }

    /// Returns the king destination square when castling (Used only for movegen)
    ///
    /// - Only accepts atomic castling rights (Like Castling::WK but NOT Castling::KING_SIDE)
    ///
    /// - Panics if composite castling rights are provided
    #[inline]
    pub(crate) fn castling_king_dest(&self, castle: Castling) -> Square {
        match castle {
            Castling::WK => Square::G1,
            Castling::WQ => Square::C1,
            Castling::BK => Square::G8,
            Castling::BQ => Square::C8,
            _ => unreachable!(),
        }
    }

    /// Returns the rook destination square when castling (Used only for movegen)
    ///
    /// - Only accepts atomic castling rights (Like Castling::WK but NOT Castling::KING_SIDE)
    ///
    /// - Panics if composite castling rights are provided
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

    /// Returns the MoveFlag for castling
    ///
    /// ## Warning
    ///
    /// - Only accepts atomic castling rights (Like Castling::WK but NOT Castling::KING_SIDE)
    ///
    /// - Panics if composite castling rights are provided
    #[inline]
    pub(crate) fn castling_flag(&self, castle: Castling) -> MoveFlag {
        match castle {
            Castling::WK | Castling::BK => MoveFlag::KingCastle,
            Castling::WQ | Castling::BQ => MoveFlag::QueenCastle,
            _ => unreachable!(),
        }
    }

    /// Returns the MoveFlag for castling
    ///
    /// - Only accepts atomic castling rights (Like Castling::WK but NOT Castling::KING_SIDE)
    ///
    /// - Panics if composite castling rights are provided
    #[inline]
    pub(crate) fn can_castle(&self, castle: Castling) -> bool {
        let us = self.stm();
        let ksq = self.ksq(us);
        let rook_sq = self.rook_sq(castle);
        let rook_dest = self.castling_rook_dest(castle);

        // The king path variable describes the path the king will go through to castle
        // Used for detecting if the king will be running into check
        let king_path = pin_bb(ksq, self.castling_king_dest(castle));
        // The rook path variable describes the path the rook will go through to castle
        // Used for detecting if the rook will be running into other pieces
        let rook_path = pin_bb(rook_sq, rook_dest);
        // Calculates the overall mobility area (which has to be empty for castling to be valid)
        let move_area = king_path | rook_path;
        // Remove the king and rook from the occupancy bitboard
        let occ = self.all_occupied_bb() ^ rook_sq.bb() ^ ksq.bb();

        // If the king path results in check or the move area is not cleared, or the rook is pinned (Chess960), the move is invalid
        (king_path & self.attacked()).is_empty()
            && (move_area & occ).is_empty()
            && !self.hv_pin().contains(rook_sq)
    }
}
