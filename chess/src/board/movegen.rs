pub mod r#gen;
pub mod init;
pub mod lookup;
pub mod magic;
pub mod move_list;

pub use lookup::{
    aligned, attacks, bishop_attacks, king_attack, knight_attack, pawn_attack, pin_bb,
    queen_attacks, rook_attacks, sq_dist,
};

pub use magic::init_magic_tables;

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

        let us = self.stm();

        let ksq = self.ksq(us);

        let rook_sq = self.rook_sq(castle);

        let rook_dest = self.castling_rook_dest(castle);

        let king_path = pin_bb(ksq, self.castling_king_dest(castle));

        let rook_path = pin_bb(rook_sq, rook_dest);

        let move_area = king_path | rook_path;

        let occ = self.all_occupied_bb() ^ rook_sq.bb() ^ ksq.bb();

        (king_path & self.attacked()).is_empty()
            && (move_area & occ).is_empty()
            && !self.hv_pin().contains(rook_sq)
    }
}
