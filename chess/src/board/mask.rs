 
 
 
 
 
 
 
 
 
use super::Board;
use super::movegen::*;
use crate::core::*;  

/******************************************\
|==========================================|
|              Castling Mask               |
|==========================================|
\******************************************/

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CastlingMask {
    pub castling: [Castling; Square::NUM],

    pub rook_sq: [Option<Square>; 4],
}

impl Default for CastlingMask {
    fn default() -> Self {
        Self {
            castling: [Castling::ALL; Square::NUM],
            rook_sq: [None, None, None, None],
        }
    }
}


impl Board {
     
     
     
     
    #[inline]
    pub(crate) fn bishop_queen_bb(&self, col: Colour) -> Bitboard {
        self.piece_bb(col, PieceType::Bishop) | self.piece_bb(col, PieceType::Queen)
    }

     
     
     
     
    #[inline]
    pub(crate) fn rook_queen_bb(&self, col: Colour) -> Bitboard {
        self.piece_bb(col, PieceType::Rook) | self.piece_bb(col, PieceType::Queen)
    }

     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
    #[inline]
    pub fn ksq(&self, col: Colour) -> Square {
         
        debug_assert!(
            !self.piece_bb(col, PieceType::King).is_empty(),
            "King must exist for colour {:?}",
            col
        );

         
        self.piece_bb(col, PieceType::King).lsb_unchecked()
    }

     
     
     
     
    #[inline]
    pub const fn attacked(&self) -> Bitboard {
        self.state.attacked
    }

     
     
     
     
     
     
     
     
     
     
     
    #[inline]
    pub const fn check_mask(&self) -> Bitboard {
        self.state.check_mask
    }

     
     
     
     
     
     
     
    #[inline]
    pub const fn diag_pin(&self) -> Bitboard {
        self.state.diag_pin
    }

     
     
     
     
     
     
     
    #[inline]
    pub const fn hv_pin(&self) -> Bitboard {
        self.state.hv_pin
    }

     
    #[inline]
    pub const fn ep_pin(&self) -> bool {
        self.state.ep_pin
    }

     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
    #[inline]
    fn calc_attacked_bb(&self) -> Bitboard {
        let us = self.stm;
        let them = !us;
         
         
        let occ = self.all_occupied_bb() ^ self.piece_bb(us, PieceType::King);

         
        let mut threatened = Bitboard::pawn_attacks(them, self.piece_bb(them, PieceType::Pawn));

         
        let knight_bb = self.piece_bb(them, PieceType::Knight);
        knight_bb.for_each(|sq| {
             
            threatened |= knight_attack(sq);
        });

         
        let bishop_queen_bb = self.bishop_queen_bb(them);
        bishop_queen_bb.for_each(|sq| {
            threatened |= bishop_attacks(sq, occ);
        });

         
        let rook_queen_bb = self.rook_queen_bb(them);
        rook_queen_bb.for_each(|sq| {
            threatened |= rook_attacks(sq, occ);
        });

        threatened |= attacks(us, PieceType::King, self.ksq(them), occ);  

        threatened
    }

     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
    #[inline]
    #[rustfmt::skip]
    fn calc_pin_mask(&self) -> (Bitboard, Bitboard) {
        let us = self.stm;
        let them = !us;
        let ksq = self.ksq(us);  

        let all_occ = self.all_occupied_bb();
        let our_occ = self.occupied_bb(us);
        let them_occ = self.occupied_bb(them);

        let mut diag_pin = Bitboard::EMPTY;
        let mut hv_pin = Bitboard::EMPTY;

         
        let probe_rays = queen_attacks(ksq, all_occ);
         
        let potential_pinned = probe_rays & our_occ;
         
        let potential_checkers = probe_rays & them_occ;
         
        let occ = all_occ ^ potential_pinned;
         

         
        let diag_pinners = bishop_attacks(ksq, occ)
            & self.bishop_queen_bb(them)
            & !potential_checkers;
        diag_pinners.for_each(|sq| diag_pin |= pin_bb(ksq, sq));

         
        let hv_pinners = rook_attacks(ksq, occ)
            & self.rook_queen_bb(them) 
            & !potential_checkers;
        hv_pinners.for_each(|sq| hv_pin |= pin_bb(ksq, sq));

        (diag_pin, hv_pin)
    }

     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
    #[inline]
    fn calc_ep_pin(&self, ep_target: Square, attackers: Bitboard) -> bool {
        let us = self.stm;
        let them = !us;
        let ksq = self.ksq(us);  
        let all_occ = self.all_occupied_bb();
        let them_occ = self.occupied_bb(them);

        let ep_target_bb = ep_target.bb();

        let potential_checkers = queen_attacks(ksq, all_occ) & them_occ;
        let occ = all_occ ^ ep_target_bb;
        let diag_pinners = bishop_attacks(ksq, occ)
            & !potential_checkers
            & self.bishop_queen_bb(them);

        if diag_pinners.is_occupied() {
            return true;
        }

        if attackers.is_singleton() {
            let ep_rank = ep_target.rank().bb();
            let occ = all_occ ^ ep_target_bb ^ attackers;
            let h_pinners = rook_attacks(ksq, occ)
                & ep_rank
                & !potential_checkers
                & self.rook_queen_bb(them);
            if h_pinners.is_occupied() {
                return true;
            }
        }

        false
    }

     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
    #[inline]
    fn calc_check_mask(&self) -> Bitboard {
        let us = self.stm;
        let ksq = self.ksq(us);
        let them = !us;
        let occ = self.all_occupied_bb();  

         
         
        let pawn_checkers = pawn_attack(us, ksq) & self.piece_bb(them, PieceType::Pawn);
         
        let knight_checkers = knight_attack(ksq) & self.piece_bb(them, PieceType::Knight);
         
        let diag_checkers = bishop_attacks(ksq, occ) & self.bishop_queen_bb(them);
        let hv_checkers = rook_attacks(ksq, occ) & self.rook_queen_bb(them);

         
        let all_checkers = pawn_checkers | knight_checkers | diag_checkers | hv_checkers;

         
        if all_checkers.is_empty() {
            Bitboard::FULL
        } else if !all_checkers.more_than_one() {
             
            let checker_sq = all_checkers.lsb_unchecked();  

             
            if (pawn_checkers | knight_checkers).contains(checker_sq) {
                 
                 
                all_checkers
            } else {
                 
                 
                pin_bb(ksq, checker_sq)  
            }
        } else {
            Bitboard::EMPTY
        }
    }

     
     
     
     
     
     
     
     
     
     
    pub(crate) fn update_masks(&mut self) {
         
        self.state.attacked = self.calc_attacked_bb();

         
        let check_mask = self.calc_check_mask();
        self.state.check_mask = check_mask;

         
         
         
         
        if !check_mask.is_empty() {
            (self.state.diag_pin, self.state.hv_pin) = self.calc_pin_mask();

            if let Some(ep_target) = self.ep_target() {
                let our_pawns = self.piece_bb(self.stm, PieceType::Pawn);

                let ep_target_bb = ep_target.bb();

                 
                let attackers = our_pawns
                    & (ep_target_bb.shift(Direction::E) | ep_target_bb.shift(Direction::W));

                if attackers.is_occupied() {
                    self.state.ep_pin = self.calc_ep_pin(ep_target, attackers);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ksq() {
        let mut board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
        board.update_masks();
        assert_eq!(board.ksq(Colour::White), Square::E1);
        assert_eq!(board.ksq(Colour::Black), Square::E8);
    }

    #[test]
    fn test_attacked_bb_initial_pos() {
         
        let mut board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
        board.update_masks();
        let attacked_by_black = board.attacked();

         
        let expected = Rank::Rank6.bb()
            | Rank::Rank7.bb()
            | Rank::Rank8.bb() ^ Square::A8.bb() ^ Square::H8.bb();

        assert_eq!(attacked_by_black, expected);
    }

    #[test]
    fn test_attacked_bb_sliders_blocked() {
         
        let mut board =
            Board::from_fen("r1b1kbnr/p1p1p1p1/8/8/8/8/P1P1P1P1/R1B1KBNR w KQkq - 0 1").unwrap();
        board.update_masks();

        let attacked_by_black = board.attacked();

         
         
        let expected = Bitboard(0x6efbbba0c0808080);

        assert_eq!(attacked_by_black, expected);
    }

    #[test]
    fn test_attacked_bb_attack_through_king() {
        let mut board = Board::from_fen("4rk2/8/8/8/8/8/8/4K3 w - - 0 1").unwrap();
        board.update_masks();

        let attacked_by_black = board.attacked();

         
        let expected = attacks(board.stm(), PieceType::Rook, Square::E8, board.all_occupied_bb())
            | attacks(board.stm(), PieceType::King, Square::F8, Bitboard::EMPTY);

        assert_eq!(attacked_by_black, expected);
    }

    #[test]
    fn test_pin_and_check_mask_no_check_no_pin() {
        let mut board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
        board.update_masks();
        assert_eq!(board.check_mask(), Bitboard::FULL);
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_and_check_mask_hv_pin() {
         
        let mut board = Board::from_fen("4rk2/8/8/8/8/8/4R3/4K3 w - - 0 1").unwrap();
        board.update_masks();
        assert_eq!(board.check_mask(), Bitboard::FULL);
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), pin_bb(Square::E1, Square::E8));  
    }

    #[test]
    fn test_pin_and_check_mask_diag_pin() {
         
        let mut board = Board::from_fen("5k2/8/8/b7/8/8/3B4/4K3 w - - 0 1").unwrap();
        board.update_masks();
        assert_eq!(board.check_mask(), Bitboard::FULL);
        assert_eq!(board.diag_pin(), pin_bb(Square::E1, Square::A5));  
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_and_check_mask_single_slider_check() {
         
        let mut board = Board::from_fen("4rk2/8/8/8/8/8/8/4K3 w - - 0 1").unwrap();
        board.update_masks();
         
        let expected_check_mask = pin_bb(Square::E1, Square::E8);
        assert_eq!(board.check_mask(), expected_check_mask);
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        println!("{}", board.hv_pin());
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_and_check_mask_single_knight_check() {
         
        let mut board = Board::from_fen("4k3/8/8/8/8/3n4/8/4K3 w - - 0 1").unwrap();
        board.update_masks();

         
        assert_eq!(board.check_mask(), Square::D3.bb());
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_and_check_mask_double_check() {
         
        let board = Board::from_fen("4rk2/8/8/8/8/6b1/8/4K3 w - - 0 1").unwrap();
         

         
        assert_eq!(board.check_mask(), Bitboard::EMPTY);
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);  
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);  
    }

    #[test]
    fn test_pin_and_check_mask_pin_and_check() {
         
        let mut board = Board::from_fen("4rk2/8/8/8/8/3n4/4R3/4K3 w - - 0 1").unwrap();
        board.update_masks();

         
        assert_eq!(board.check_mask(), Square::D3.bb());
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
         
        assert_eq!(board.hv_pin(), Bitboard(0x1010101010101000));
    }

    #[test]
    fn test_mask_multiple_pins() {
         
         
        let mut board = Board::from_fen("r6k/8/8/8/3b4/N7/1R6/K1B4q w - - 0 1").unwrap();
        board.update_masks();

        let ksq = Square::A1;

         
         
        let expected_diag_pin = pin_bb(ksq, Square::D4);  
        let expected_hv_pin = pin_bb(ksq, Square::A8) | pin_bb(ksq, Square::H1);  

        assert_eq!(board.check_mask(), Bitboard::FULL, "Should not be in check");
        assert_eq!(
            board.diag_pin(),
            expected_diag_pin,
            "Knight D2 should be diagonally pinned"
        );
        assert_eq!(
            board.hv_pin(),
            expected_hv_pin,
            "Rook D3 should be horizontally pinned"
        );
    }

    #[test]
    fn test_mask_pin_obstructed() {
         
         
        let mut board = Board::from_fen("r6k/8/8/8/8/8/4R3/R3K2R w KQ - 0 1").unwrap();
        board.update_masks();

        assert_eq!(board.check_mask(), Bitboard::FULL, "Should not be in check");
        assert_eq!(
            board.diag_pin(),
            Bitboard::EMPTY,
            "No diagonal pins expected"
        );
        assert_eq!(
            board.hv_pin(),
            Bitboard::EMPTY,
            "Rook E2 should not be pinned by A8"
        );
    }

    #[test]
    fn test_mask_pawn_check() {
         
        let mut board = Board::from_fen("4k3/3P4/8/8/8/8/8/4K3 b - - 0 1").unwrap();
        board.update_masks();  

        assert_eq!(
            board.check_mask(),
            Square::D7.bb(),
            "Check mask should be pawn at D7"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }
    #[test]
    fn test_mask_pawn_check_white() {
         
        let mut board = Board::from_fen("4k3/8/8/8/8/8/3p4/4K3 w - - 0 1").unwrap();
        board.update_masks();  

        assert_eq!(
            board.check_mask(),
            Square::D2.bb(),
            "Check mask should be pawn at D2"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_mask_queen_check_diagonal() {
         
        let mut board = Board::from_fen("4k3/8/8/8/1q6/8/8/4K3 w - - 0 1").unwrap();
        board.update_masks();

        let expected_check_mask = pin_bb(Square::E1, Square::B4);  
        assert_eq!(
            board.check_mask(),
            expected_check_mask,
            "Check mask should be line from B4"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_mask_queen_check_horizontal() {
         
        let mut board = Board::from_fen("4k3/8/8/8/8/8/8/4K2q w - - 0 1").unwrap();
        board.update_masks();

        let expected_check_mask = pin_bb(Square::E1, Square::H1);  
        assert_eq!(
            board.check_mask(),
            expected_check_mask,
            "Check mask should be line from H1"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_mask_double_check_knight_rook() {
         
        let mut board = Board::from_fen("4rk2/8/8/8/8/3n4/8/4K3 w - - 0 1").unwrap();
        board.update_masks();

         
        assert_eq!(
            board.check_mask(),
            Bitboard::EMPTY,
            "Check mask should be empty in double check"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY, "No pins in double check");
        assert_eq!(board.hv_pin(), Bitboard::EMPTY, "No pins in double check");
    }

    #[test]
    fn test_mask_stalemate_position() {
         
        let mut board = Board::from_fen("k7/8/KQ6/8/8/8/8/8 b - - 0 1").unwrap();
        board.update_masks();  

        assert_eq!(
            board.check_mask(),
            Bitboard::FULL,
            "Should not be in check (stalemate)"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);

         
        let attacked_by_white = board.attacked();
        assert!(attacked_by_white.contains(Square::A7));  
        assert!(attacked_by_white.contains(Square::B7));  
        assert!(attacked_by_white.contains(Square::B8));  
    }

    #[test]
    fn test_mask_checkmate_position_back_rank() {
         
        let mut board = Board::from_fen("4k2R/8/8/8/8/8/8/6K1 b - - 0 1").unwrap();
        board.update_masks();  

        let expected_check_mask = pin_bb(Square::E8, Square::H8);  
        assert_eq!(
            board.check_mask(),
            expected_check_mask,
            "Check mask should be line from Rook H8"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);

         
        let attacked_by_white = board.attacked();
        assert!(attacked_by_white.contains(Square::E8));
        assert!(attacked_by_white.contains(Square::F8));  
        assert!(attacked_by_white.contains(Square::G8));  
    }

    #[test]
    fn test_mask_castling_squares_attacked() {
         
        let mut board = Board::from_fen("r3k2r/2Q5/8/8/8/8/8/R3K2R b KQkq - 0 1").unwrap();
        board.update_masks();  

        let attacked_by_white = board.attacked();

         
        assert!(attacked_by_white.contains(Square::B8), "B8 attacked by Ra1");
        assert!(attacked_by_white.contains(Square::C8), "C8 attacked by Ra1");
        assert!(attacked_by_white.contains(Square::D8), "D8 attacked by Ra1");
         
        assert!(
            !attacked_by_white.contains(Square::F8),
            "F8 not attacked by Ra1"
        );
        assert!(
            !attacked_by_white.contains(Square::G8),
            "G8 not attacked by Ra1"
        );

         
        let mut board_white_turn = Board::from_fen("4krr1/8/8/8/8/8/8/R3K2R w KQ - 0 1").unwrap();
        board_white_turn.update_masks();  
        let attacked_by_black = board_white_turn.state.attacked;

         
        assert!(attacked_by_black.contains(Square::F1), "F1 attacked by Rh8");
        assert!(attacked_by_black.contains(Square::G1), "G1 attacked by Rh8");
         
        assert!(
            !attacked_by_black.contains(Square::B1),
            "B1 not attacked by Rh8"
        );
        assert!(
            !attacked_by_black.contains(Square::C1),
            "C1 not attacked by Rh8"
        );
        assert!(
            !attacked_by_black.contains(Square::D1),
            "D1 not attacked by Rh8"
        );
    }

    #[test]
    fn test_mask_pin_prevents_check() {
         
         
        let mut board = Board::from_fen("4rk2/8/b7/8/8/8/4R3/4K3 w - - 0 1").unwrap();
        board.update_masks();

        let expected_hv_pin = pin_bb(Square::E1, Square::E8);  

        assert_eq!(board.check_mask(), Bitboard::FULL, "Should not be in check");
        assert_eq!(board.diag_pin(), Bitboard::EMPTY, "No diagonal pin");
        assert_eq!(board.hv_pin(), expected_hv_pin, "Rook E2 should be pinned");
        assert!(board.hv_pin().contains(Square::E2));
    }

    fn get_ep_pin(fen: &str) -> bool {
        let mut board = Board::from_fen(fen).unwrap();
        board.update_masks();
        board.ep_pin()
    }

    #[test]
    fn test_ep_h_pin() {
        assert!(get_ep_pin("2k5/8/8/K2pP2r/8/8/8/8 w - d6 0 1"));
        assert!(!get_ep_pin("2k5/8/8/K2pPP1r/8/8/8/8 w - d6 0 1"));
        assert!(!get_ep_pin("2k5/8/8/K1PpP2r/8/8/8/8 w - d6 0 1"));
        assert!(!get_ep_pin("2k5/8/8/KP1pP2r/8/8/8/8 w - d6 0 1"));
        assert!(get_ep_pin("2k5/8/8/K4pPr/8/8/8/8 w - f6 0 1"));
        assert!(!get_ep_pin("2k5/8/8/K2r1pP1/8/8/8/8 w - - 0 1"));
        assert!(!get_ep_pin("2k5/8/8/K1n2pPr/8/8/8/8 w - f6 0 1"));
        assert!(!get_ep_pin("2k5/8/8/K2N1pPr/8/8/8/8 w - f6 0 1"));
        assert!(!get_ep_pin("2k5/8/1K6/5pPq/8/8/8/8 w - f6 0 1"));
    }

    #[test]
    fn test_ep_diag_pin() {
        assert!(get_ep_pin("5b2/7k/8/2pP4/8/K7/8/8 w - c6 0 1"));
        assert!(get_ep_pin("5b2/7k/8/1Pp5/8/K7/8/8 w - c6 0 1"));
        assert!(!get_ep_pin("5b2/7k/8/2p5/1P6/K7/8/8 w - c6 0 1"));
        assert!(!get_ep_pin("5b2/7k/3P4/2p5/8/K7/8/8 w - c6 0 1"));
        assert!(!get_ep_pin("5b2/7k/3P4/2pP4/8/K7/8/8 w - c6 0 1"));
        assert!(get_ep_pin("5q2/7k/8/2pP4/8/K7/8/8 w - c6 0 1"));
        assert!(!get_ep_pin("5q2/7k/3N4/2pP4/8/K7/8/8 w - c6 0 1"));
        assert!(!get_ep_pin("5q2/7k/8/2pP4/1B6/K7/8/8 w - c6 0 1"));
        assert!(!get_ep_pin("5q2/7k/8/2pP4/1r6/K7/8/8 w - c6 0 1"));
    }
}
