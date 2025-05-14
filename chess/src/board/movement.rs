use super::Board;
use super::attacks;
use super::movegen::aligned;
use crate::core::*;

impl Board {
    #[inline]
    pub(crate) fn add_piece(&mut self, piece: Piece, square: Square) {
        self.board[square.index()] = Some(piece);

        self.pieces[piece.pt().index()].set(square);
        self.occupied[piece.colour().index()].set(square);
    }

    #[inline]
    pub(crate) fn remove_piece(&mut self, square: Square) {
        debug_assert!(self.on(square).is_some(), "remove_piece: 'square' is empty");
        let piece = unsafe { self.on(square).unwrap_unchecked() };

        self.board[square.index()] = None;

        self.pieces[piece.pt().index()].clear(square);
        self.occupied[piece.colour().index()].clear(square);
    }

    #[inline]
    pub(crate) fn move_piece(&mut self, from: Square, to: Square) {
        debug_assert!(
            self.on(from).is_some(),
            "move_piece: 'from' square is empty"
        );
        let piece = unsafe { self.on(from).unwrap_unchecked() };

        self.board[from.index()] = None;

        self.board[to.index()] = Some(piece);

        self.pieces[piece.pt().index()].clear(from);
        self.pieces[piece.pt().index()].set(to);

        self.occupied[piece.colour().index()].clear(from);
        self.occupied[piece.colour().index()].set(to);
    }

    #[inline]
    fn set_ep(&mut self, from: Square) {
        let us = self.stm;

        self.state.enpassant = Some(unsafe { from.add_unchecked(us.forward()) });

        debug_assert!(
            self.state.enpassant.is_some(),
            "En passant square should exist after double push"
        );
        self.state
            .keys
            .toggle_ep(unsafe { self.state.enpassant.unwrap_unchecked().file() });
    }

    #[inline]
    fn rook_from(&self, king_side: bool) -> Square {
        let us = self.stm;
        let index = us.index() * 2 + !king_side as usize;

        debug_assert!(
            self.castling_mask.rook_sq[index].is_some(),
            "If Castling Rights are set then there should be a rook square set"
        );
        unsafe { self.castling_mask.rook_sq[index].unwrap_unchecked() }
    }

    #[inline]
    fn rook_to(&self, king_side: bool) -> Square {
        let us = self.stm;
        match king_side {
            true => Square::F1.relative(us),
            false => Square::D1.relative(us),
        }
    }

    #[inline]
    fn castle(&mut self, king_side: bool) {
        let us = self.stm;
        let piece = Piece::from_parts(us, PieceType::Rook);

        let rook_from = self.rook_from(king_side);
        let rook_to = self.rook_to(king_side);

        self.move_piece(rook_from, rook_to);

        self.state.keys.toggle_piece(piece, rook_from);
        self.state.keys.toggle_piece(piece, rook_to);
    }

    #[inline]
    fn undo_castle(&mut self, king_side: bool) {
        let rook_from = self.rook_from(king_side);
        let rook_to = self.rook_to(king_side);

        self.move_piece(rook_to, rook_from);
    }

    #[inline]
    fn update_castle_rights(&mut self, from: Square, to: Square) {
        self.state.keys.toggle_castle(self.state.castle);

        self.state
            .castle
            .mask(self.castling_rights(from) & self.castling_rights(to));

        self.state.keys.toggle_castle(self.state.castle);
    }

    #[inline]
    fn update_repetitions(&mut self) {
        self.state.repetitions = 0;

        let roll_back: usize = 1 + self.state.fifty_move as usize;

        let iterator = self
            .history
            .iter()
            .rev()
            .enumerate()
            .take(roll_back)
            .skip(1)
            .step_by(2);

        for (idx, state) in iterator {
            if state.keys.key == self.state.keys.key {
                if state.repetitions == 0 {
                    self.state.repetitions = idx as i8;
                } else {
                    self.state.repetitions = -(idx as i8);
                }
                break;
            }
        }
    }

    fn store_state(&mut self) {
        let state = self.state.snapshot();
        let old = std::mem::replace(&mut self.state, state);
        self.history.push(old);
    }

    pub fn make_move(&mut self, move_: Move) {
        self.store_state();

        self.half_moves += 1;

        let from = move_.from();
        let to = move_.to();
        let us = self.stm;
        let them = !us;

        let piece = unsafe { self.on(from).unwrap_unchecked() };
        let flag = move_.flag();

        self.state.fifty_move += 1;

        if let Some(ep_sq) = self.state.enpassant {
            self.state.keys.toggle_ep(ep_sq.file());
            self.state.enpassant = None;
        }

        match flag {
            MoveFlag::QuietMove => {
                if piece.pt() == PieceType::Pawn {
                    self.state.fifty_move = 0;
                }

                self.move_piece(from, to);

                self.state.keys.toggle_piece(piece, from);
                self.state.keys.toggle_piece(piece, to);

                self.update_castle_rights(from, to);
            }

            MoveFlag::DoublePawnPush => {
                self.state.fifty_move = 0;

                self.set_ep(from);

                self.move_piece(from, to);

                self.state.keys.toggle_piece(piece, from);
                self.state.keys.toggle_piece(piece, to);
            }

            MoveFlag::KingCastle | MoveFlag::QueenCastle => {
                self.remove_piece(from);

                self.state.keys.toggle_piece(piece, from);
                let is_king_side = flag == MoveFlag::KingCastle;

                self.castle(is_king_side);

                self.add_piece(piece, to);

                self.state.keys.toggle_piece(piece, to);

                self.update_castle_rights(from, to);
            }

            MoveFlag::Capture => {
                self.state.fifty_move = 0;

                debug_assert!(
                    self.on(to).is_some(),
                    "make_move: Capture flag set, but 'to' square is empty"
                );
                let captured_piece = unsafe { self.on(to).unwrap_unchecked() };
                self.state.captured = Some(captured_piece);

                self.remove_piece(to);

                self.state.keys.toggle_piece(captured_piece, to);

                self.move_piece(from, to);

                self.state.keys.toggle_piece(piece, from);
                self.state.keys.toggle_piece(piece, to);

                self.update_castle_rights(from, to);
            }

            MoveFlag::EPCapture => {
                self.state.fifty_move = 0;

                debug_assert!(
                    to.add(-us.forward()).is_ok(),
                    "make_move: Invalid EP target square calculation"
                );
                let cap_sq = unsafe { to.add(-us.forward()).unwrap_unchecked() };
                let captured_pawn = Piece::from_parts(them, PieceType::Pawn);

                self.state.captured = Some(captured_pawn);

                self.remove_piece(cap_sq);

                self.state.keys.toggle_piece(captured_pawn, cap_sq);

                self.move_piece(from, to);

                self.state.keys.toggle_piece(piece, from);
                self.state.keys.toggle_piece(piece, to);
            }

            MoveFlag::KnightPromo
            | MoveFlag::BishopPromo
            | MoveFlag::RookPromo
            | MoveFlag::QueenPromo => {
                self.state.fifty_move = 0;

                let promo_pt = move_.promotion_pt();
                let promo_piece = Piece::from_parts(us, promo_pt);

                self.remove_piece(from);

                self.state.keys.toggle_piece(piece, from);

                self.add_piece(promo_piece, to);

                self.state.keys.toggle_piece(promo_piece, to);

                self.update_castle_rights(from, to);
            }

            MoveFlag::KnightPromoCapture
            | MoveFlag::BishopPromoCapture
            | MoveFlag::RookPromoCapture
            | MoveFlag::QueenPromoCapture => {
                self.state.fifty_move = 0;

                debug_assert!(
                    self.on(to).is_some(),
                    "make_move: PromoCapture flag set, but 'to' square is empty"
                );
                let captured_piece = unsafe { self.on(to).unwrap_unchecked() };
                self.state.captured = Some(captured_piece);

                self.remove_piece(to);

                self.state.keys.toggle_piece(captured_piece, to);

                let promo_pt = move_.promotion_pt();
                let promo_piece = Piece::from_parts(us, promo_pt);

                self.remove_piece(from);

                self.state.keys.toggle_piece(piece, from);

                self.add_piece(promo_piece, to);

                self.state.keys.toggle_piece(promo_piece, to);

                self.update_castle_rights(from, to);
            }
        }

        self.stm = !self.stm;

        self.state.keys.toggle_side();

        self.update_masks();

        self.update_repetitions();
    }

    pub fn undo_move(&mut self, move_: Move) {
        self.stm = !self.stm;

        self.half_moves -= 1;

        let from = move_.from();
        let to = move_.to();
        let us = self.stm;
        let flag = move_.flag();
        let captured = self.state.captured;

        self.state = self.history.pop().unwrap();

        match flag {
            MoveFlag::QuietMove | MoveFlag::DoublePawnPush => {
                self.move_piece(to, from);
            }

            MoveFlag::Capture => {
                self.move_piece(to, from);

                debug_assert!(
                    captured.is_some(),
                    "undo_move: Capture flag set, but restored state has no captured piece"
                );
                self.add_piece(unsafe { captured.unwrap_unchecked() }, to);
            }

            MoveFlag::EPCapture => {
                self.move_piece(to, from);

                debug_assert!(
                    to.add(-us.forward()).is_ok(),
                    "undo_move: Invalid EP target square calculation"
                );
                let cap_sq = unsafe { to.add(-us.forward()).unwrap_unchecked() };

                debug_assert!(
                    captured.is_some(),
                    "undo_move: EPCapture flag set, but restored state has no captured piece"
                );
                self.add_piece(unsafe { captured.unwrap_unchecked() }, cap_sq);
            }

            MoveFlag::KingCastle | MoveFlag::QueenCastle => {
                let is_king_side = flag == MoveFlag::KingCastle;

                self.remove_piece(to);

                self.undo_castle(is_king_side);

                self.add_piece(Piece::from_parts(us, PieceType::King), from);
            }

            MoveFlag::KnightPromo
            | MoveFlag::BishopPromo
            | MoveFlag::RookPromo
            | MoveFlag::QueenPromo => {
                self.remove_piece(to);

                self.add_piece(Piece::from_parts(us, PieceType::Pawn), from);
            }

            MoveFlag::KnightPromoCapture
            | MoveFlag::BishopPromoCapture
            | MoveFlag::RookPromoCapture
            | MoveFlag::QueenPromoCapture => {
                self.remove_piece(to);

                debug_assert!(
                    captured.is_some(),
                    "undo_move: PromoCapture flag set, but restored state has no captured piece"
                );
                self.add_piece(unsafe { captured.unwrap_unchecked() }, to);

                self.add_piece(Piece::from_parts(us, PieceType::Pawn), from);
            }
        }
    }

    pub fn make_null_move(&mut self) {
        self.store_state();

        if self.state.enpassant.is_some() {
            self.state
                .keys
                .toggle_ep(unsafe { self.ep_target().unwrap_unchecked().file() });
            self.state.enpassant = None;
        }

        self.state.fifty_move = 0;

        self.state.keys.toggle_side();

        self.stm = !self.stm;

        self.update_masks();
    }

    pub fn undo_null_move(&mut self) {
        self.stm = !self.stm;

        self.state = self.history.pop().unwrap();
    }

    pub fn is_legal(&self, move_: Move) -> bool {
        if !move_.is_valid() {
            return false;
        }

        let us = self.stm;

        let from = move_.from();
        let to = move_.to();
        let all_occ = self.all_occupied_bb();

        let captured = self.on(to);
        let is_pawn_move = move_.is_double_push() || move_.is_promotion() || move_.is_ep_capture();

        if !self.on(from).is_some_and(|p| p.colour() == us) || (!move_.is_castle() && from == to) {
            return false;
        }

        if !move_.is_castle() && captured.is_some_and(|p| p.colour() == us) {
            return false;
        }

        let piece = self.on(from).unwrap();
        let piece_type = piece.pt();

        if (move_.is_capture() != captured.is_some()
            && !move_.is_ep_capture()
            && !move_.is_castle())
            || (is_pawn_move && piece_type != PieceType::Pawn)
        {
            return false;
        }

        if move_.is_promotion() && from.relative(us).rank() != Rank::Rank7 {
            return false;
        }

        if move_.is_castle() {
            if to.file() == File::FileG && self.castling().has(Castling::king_side(us)) {
                return !self.in_check() && self.can_castle(Castling::king_side(us));
            }

            if to.file() == File::FileC && self.castling().has(Castling::queen_side(us)) {
                return !self.in_check() && self.can_castle(Castling::queen_side(us));
            }

            return false;
        }

        if piece_type != PieceType::Pawn {
            if !attacks(us, piece_type, from, all_occ).contains(to) {
                return false;
            }
        } else {
            if move_.is_ep_capture() {
                if self.ep_target().is_none() || self.ep_pin() {
                    return false;
                }
                let ep_target = self.ep_target().unwrap();

                if to != ep_target
                    && (attacks(us, PieceType::Pawn, from, Bitboard::EMPTY) & to.bb()).is_empty()
                {
                    return false;
                }
            } else if !move_.is_capture() {
                if move_.is_double_push() && from.relative(us).rank() != Rank::Rank2 {
                    return false;
                }

                let forward = us.forward();
                let push_sq = unsafe { from.add_unchecked(forward) };

                if all_occ.contains(push_sq) || (!move_.is_double_push() && push_sq != to) {
                    return false;
                }

                if move_.is_double_push() {
                    let double_push_sq = unsafe { push_sq.add_unchecked(forward) };
                    if double_push_sq != to || all_occ.contains(double_push_sq) {
                        return false;
                    }
                }
            }
        }

        if piece_type != PieceType::King {
            if !self.check_mask().contains(to)
                && !(move_.is_ep_capture() && self.check_mask().contains(self.ep_target().unwrap()))
            {
                return false;
            }
        } else {
            return !self.attacked().contains(to);
        }

        let diag_pin = self.diag_pin();
        let hv_pin = self.hv_pin();

        let ksq = self.ksq(us);

        let diag_pinned =
            diag_pin.contains(from) && diag_pin.contains(to) && aligned(from, to, ksq);

        let hv_pinned = hv_pin.contains(from) && hv_pin.contains(to) && aligned(from, to, ksq);

        let not_pinned = !(diag_pin.contains(from) || hv_pin.contains(from));

        return diag_pinned || hv_pinned || not_pinned;
    }

    pub fn is_capture(&self, move_: Move) -> bool {
        move_.is_valid() && self.on(move_.to()).is_some()
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::board::fen::*;

    fn board_from_fen(fen: &str) -> Board {
        let board = Board::from_fen(fen).expect("Test FEN should be valid");

        let calculated_key = board.calc_key();
        let calculated_pawn_key = board.calc_pawn_key();
        let calculated_non_pawn_key = board.calc_non_pawn_key();
        assert_eq!(
            board.state.keys.key, calculated_key,
            "Key mismatch after initial FEN parse for: {}",
            fen
        );
        assert_eq!(
            board.state.keys.pawn_key, calculated_pawn_key,
            "Pawn key mismatch after initial FEN parse for: {}",
            fen
        );
        assert_eq!(
            board.state.keys.non_pawn_key[0], calculated_non_pawn_key[0],
            "Pawn key mismatch after initial FEN parse for: {}",
            fen
        );
        assert_eq!(
            board.state.keys.non_pawn_key[1], calculated_non_pawn_key[1],
            "Pawn key mismatch after initial FEN parse for: {}",
            fen
        );
        board
    }

    fn test_make_undo(fen_before: &str, move_to_test: Move, fen_after: &str) {
        let mut board = board_from_fen(fen_before);
        let key_before = board.state.keys.key;
        let pawn_key_before = board.state.keys.pawn_key;
        let non_pawn_key_before = board.state.keys.non_pawn_key;

        board.make_move(move_to_test);
        let key_after_make = board.state.keys.key;
        let pawn_key_after_make = board.state.keys.pawn_key;
        let non_pawn_key_after_make = board.state.keys.non_pawn_key;

        assert_eq!(board.fen(), fen_after, "FEN mismatch after make_move for",);

        assert_ne!(
            key_before, key_after_make,
            "Key should change after make_move for",
        );

        let expected_pawn_key_after = Board::from_fen(fen_after).unwrap().calc_pawn_key();
        assert_eq!(
            pawn_key_after_make, expected_pawn_key_after,
            "Pawn key incorrect after make_move for. Expected {:?}, Got {:?}",
            expected_pawn_key_after, pawn_key_after_make
        );
        let expected_non_pawn_key_after = Board::from_fen(fen_after).unwrap().calc_non_pawn_key();
        assert_eq!(
            non_pawn_key_after_make, expected_non_pawn_key_after,
            "Pawn key incorrect after make_move for. Expected {:?}, Got {:?}",
            expected_non_pawn_key_after, non_pawn_key_after_make
        );

        let piece = Board::from_fen(fen_before)
            .unwrap()
            .on(move_to_test.from())
            .unwrap();
        let captured_piece = if move_to_test.flag().is_capture() {
            Board::from_fen(fen_before).unwrap().on(move_to_test.to())
        } else if move_to_test.is_ep_capture() {
            Some(Piece::from_parts(!board.stm(), PieceType::Pawn))
        } else {
            None
        };

        let affects_pawn_key = piece.pt() == PieceType::Pawn
            || captured_piece.map_or_else(|| false, |p| p.pt() == PieceType::Pawn);
        if affects_pawn_key {
            assert_ne!(
                pawn_key_before, pawn_key_after_make,
                "Pawn key should change after pawn-related move",
            );
        } else {
            assert_eq!(
                pawn_key_before, pawn_key_after_make,
                "Pawn key should NOT change after non-pawn move",
            );
        }

        board.undo_move(move_to_test);
        let key_after_undo = board.state.keys.key;
        let pawn_key_after_undo = board.state.keys.pawn_key;
        let non_pawn_key_after_undo = board.state.keys.non_pawn_key;

        assert_eq!(board.fen(), fen_before, "FEN mismatch after undo_move for",);

        assert_eq!(
            key_after_undo, key_before,
            "Key mismatch after undo_move for",
        );
        assert_eq!(
            pawn_key_after_undo, pawn_key_before,
            "Pawn key mismatch after undo_move for",
        );
        assert_eq!(
            non_pawn_key_after_undo, non_pawn_key_before,
            "Non Pawn key mismatch after undo_move for",
        );

        let calculated_key_after_undo = board.calc_key();
        let calculated_pawn_key_after_undo = board.calc_pawn_key();
        assert_eq!(
            calculated_key_after_undo, key_before,
            "Recalculated key mismatch after undo_move for",
        );
        assert_eq!(
            calculated_pawn_key_after_undo, pawn_key_before,
            "Recalculated pawn key mismatch after undo_move for",
        );
    }

    #[test]
    fn test_quiet_pawn_move() {
        test_make_undo(
            START_FEN,
            Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush),
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
        );
    }

    #[test]
    fn test_quiet_knight_move() {
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
            Move::new(Square::G8, Square::F6, MoveFlag::QuietMove),
            "rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2",
        );
    }

    #[test]
    fn test_capture() {
        test_make_undo(
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2",
            Move::new(Square::E4, Square::D5, MoveFlag::Capture),
            "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2",
        );
    }

    #[test]
    fn test_en_passant_capture() {
        let fen_before_ep = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3";
        test_make_undo(
            fen_before_ep,
            Move::new(Square::E5, Square::D6, MoveFlag::EPCapture),
            "rnbqkbnr/ppp1pppp/3P4/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3",
        );
    }

    #[test]
    fn test_black_en_passant_capture() {
        let fen_before_ep = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let mut board = board_from_fen(fen_before_ep);
        board.make_move(Move::new(Square::D2, Square::D4, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::F7, Square::F5, MoveFlag::QuietMove));
        board.make_move(Move::new(Square::D4, Square::D5, MoveFlag::QuietMove));
        board.make_move(Move::new(Square::E7, Square::E5, MoveFlag::DoublePawnPush));

        let fen_before_black_ep = board.fen();
        assert_eq!(
            fen_before_black_ep,
            "rnbqkbnr/pppp2pp/8/3Ppp2/8/8/PPP1PPPP/RNBQKBNR w KQkq e6 0 3"
        );

        test_make_undo(
            &fen_before_black_ep,
            Move::new(Square::D5, Square::E6, MoveFlag::EPCapture),
            "rnbqkbnr/pppp2pp/4P3/5p2/8/8/PPP1PPPP/RNBQKBNR b KQkq - 0 3",
        );
    }

    #[test]
    fn test_white_kingside_castle() {
        let fen_before_castle = "rnbq1bnr/pppppkpp/8/8/8/8/PPPPPPPP/RNBQK2R w KQ - 0 5";
        test_make_undo(
            fen_before_castle,
            Move::new(Square::E1, Square::G1, MoveFlag::KingCastle),
            "rnbq1bnr/pppppkpp/8/8/8/8/PPPPPPPP/RNBQ1RK1 b - - 1 5",
        );
    }

    #[test]
    fn test_black_queenside_castle() {
        let fen_before_castle = "r3kbnr/p1pp1ppp/bpn1p3/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 7";
        test_make_undo(
            fen_before_castle,
            Move::new(Square::E8, Square::C8, MoveFlag::QueenCastle),
            "2kr1bnr/p1pp1ppp/bpn1p3/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 1 8",
        );
    }

    #[test]
    fn test_promotion_quiet() {
        let fen_before_promo = "r1bqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6";
        test_make_undo(
            fen_before_promo,
            Move::new_promotion(Square::B7, Square::B8, PieceType::Queen, false),
            "rQbqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 6",
        );
    }

    #[test]
    fn test_promotion_capture() {
        let fen_before_promo_cap = "r1bqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6";
        test_make_undo(
            fen_before_promo_cap,
            Move::new_promotion(Square::B7, Square::A8, PieceType::Knight, true),
            "N1bqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQk - 0 6",
        );
    }

    #[test]
    fn test_castling_rights_king_move() {
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1",
            Move::new(Square::E1, Square::E2, MoveFlag::QuietMove),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPKPPP/RNBQ1BNR b kq - 1 1",
        );
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR w KQkq - 0 1",
            Move::new(Square::E1, Square::D1, MoveFlag::QuietMove),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBK1BNR b kq - 1 1",
        );
    }

    #[test]
    fn test_castling_rights_rook_move() {
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 1",
            Move::new(Square::A1, Square::A2, MoveFlag::QuietMove),
            "rnbqkbnr/pppppppp/8/8/8/8/RPPPPPPP/1NBQKBNR b Kkq - 1 1",
        );
        test_make_undo(
            "rnbqkbnr/ppppppp1/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1",
            Move::new(Square::H8, Square::H6, MoveFlag::QuietMove),
            "rnbqkbn1/ppppppp1/7r/8/8/8/PPPPPPPP/RNBQKBNR w KQq - 1 2",
        );
    }

    #[test]
    fn test_castling_rights_rook_capture() {
        let fen_before_capture = "rnbqkbnr/1ppppppp/8/p7/P7/8/1PPPPPPP/RNBQKBNR w KQkq - 0 3";
        let mut board = Board::from_fen(fen_before_capture).expect("Test FEN should be valid");
        board.make_move(Move::new(Square::B2, Square::B4, MoveFlag::QuietMove));
        board.make_move(Move::new(Square::A5, Square::B4, MoveFlag::Capture));
        board.make_move(Move::new(Square::A8, Square::A4, MoveFlag::Capture));

        let fen_before_capture = "rnbqkbnr/pppppppp/1N6/8/8/8/PPPPPPPP/R1BQKBNR w KQkq - 0 1";
        let mut board_simple =
            Board::from_fen(fen_before_capture).expect("Test FEN should be valid");
        board_simple.make_move(Move::new(Square::B6, Square::A8, MoveFlag::Capture));

        let fen_after_capture = board_simple.fen();
        assert_eq!(
            fen_after_capture,
            "Nnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R1BQKBNR b KQk - 0 1"
        );

        test_make_undo(
            "rnbqkbnr/pppppppp/1N6/8/8/8/PPPPPPPP/R1BQKBNR w KQkq - 0 1",
            Move::new(Square::B6, Square::A8, MoveFlag::Capture),
            "Nnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R1BQKBNR b KQk - 0 1",
        );
    }

    #[test]
    fn test_fifty_move_counter() {
        let mut board = board_from_fen(START_FEN);
        assert_eq!(board.state.fifty_move, 0);

        board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
        assert_eq!(board.state.fifty_move, 0);
        board.undo_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));

        board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::D7, Square::D5, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::E4, Square::D5, MoveFlag::Capture));
        assert_eq!(board.state.fifty_move, 0);
        board.undo_move(Move::new(Square::E4, Square::D5, MoveFlag::Capture));
        board.undo_move(Move::new(Square::D7, Square::D5, MoveFlag::DoublePawnPush));
        board.undo_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));

        board.make_move(Move::new(Square::G1, Square::F3, MoveFlag::QuietMove));
        assert_eq!(board.state.fifty_move, 1);
        board.undo_move(Move::new(Square::G1, Square::F3, MoveFlag::QuietMove));

        board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::E7, Square::E5, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::E1, Square::E2, MoveFlag::QuietMove));
        assert_eq!(board.state.fifty_move, 1);
    }

    #[test]
    fn test_quiet_pawn_move_keys() {
        test_make_undo(
            START_FEN,
            Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush),
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
        );
    }

    #[test]
    fn test_quiet_knight_move_keys() {
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
            Move::new(Square::G8, Square::F6, MoveFlag::QuietMove),
            "rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2",
        );
    }

    #[test]
    fn test_capture_pawn_takes_pawn_keys() {
        test_make_undo(
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2",
            Move::new(Square::E4, Square::D5, MoveFlag::Capture),
            "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2",
        );
    }

    #[test]
    fn test_capture_knight_takes_pawn_keys() {
        test_make_undo(
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
            Move::new(Square::F3, Square::D4, MoveFlag::QuietMove),
            "rnbqkbnr/ppp1pppp/8/3p4/3NP3/8/PPPP1PPP/RNBQKB1R w KQkq - 2 3",
        );

        test_make_undo(
            "r1bqkbnr/ppp1pppp/2n5/3p4/3NP3/8/PPPP1PPP/RNBQKB1R b KQkq - 3 3",
            Move::new(Square::C6, Square::D4, MoveFlag::Capture),
            "r1bqkbnr/ppp1pppp/8/3p4/3nP3/8/PPPP1PPP/RNBQKB1R w KQkq - 0 4",
        );
    }

    #[test]
    fn test_en_passant_capture_keys() {
        let fen_before_ep = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3";
        test_make_undo(
            fen_before_ep,
            Move::new(Square::E5, Square::D6, MoveFlag::EPCapture),
            "rnbqkbnr/ppp1pppp/3P4/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3",
        );
    }

    #[test]
    fn test_white_kingside_castle_keys() {
        let fen_before_castle = "rnbq1bnr/pppppkpp/8/8/8/8/PPPPPPPP/RNBQK2R w KQ - 0 5";
        test_make_undo(
            fen_before_castle,
            Move::new(Square::E1, Square::G1, MoveFlag::KingCastle),
            "rnbq1bnr/pppppkpp/8/8/8/8/PPPPPPPP/RNBQ1RK1 b - - 1 5",
        );
    }

    #[test]
    fn test_promotion_quiet_keys() {
        let fen_before_promo = "r1bqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6";
        test_make_undo(
            fen_before_promo,
            Move::new_promotion(Square::B7, Square::B8, PieceType::Queen, false),
            "rQbqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 6",
        );
    }

    #[test]
    fn test_promotion_capture_pawn_keys() {
        let fen_before_promo_cap = "rnbqkbnr/ppPppppp/8/8/8/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1";
        test_make_undo(
            fen_before_promo_cap,
            Move::new_promotion(Square::C7, Square::D8, PieceType::Queen, true),
            "rnbQkbnr/pp1ppppp/8/8/8/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1",
        );
    }

    #[test]
    fn test_promotion_capture_non_pawn_keys() {
        let fen_before_promo_cap = "rnbqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6";
        test_make_undo(
            fen_before_promo_cap,
            Move::new_promotion(Square::B7, Square::A8, PieceType::Knight, true),
            "Nnbqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQk - 0 6",
        );
    }

    #[test]
    fn test_xfen_white_kingside_castle_k_e1_r_g1() {
        let fen_before = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/4KR2 w K - 0 1";

        let castle_move = Move::new(Square::E1, Square::G1, MoveFlag::KingCastle);

        let fen_after = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/5RK1 b - - 1 1";
        test_make_undo(fen_before, castle_move, fen_after);
    }

    #[test]
    fn test_xfen_white_queenside_castle_k_e1_r_b1() {
        let fen_before = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/1R2K3 w Q - 0 1";
        let castle_move = Move::new(Square::E1, Square::C1, MoveFlag::QueenCastle);
        let fen_after = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/2KR4 b - - 1 1";
        test_make_undo(fen_before, castle_move, fen_after);
    }

    #[test]
    fn test_xfen_black_kingside_castle_k_e8_r_f8() {
        let fen_before = "4kr2/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b k - 0 1";

        let castle_move = Move::new(Square::E8, Square::G8, MoveFlag::KingCastle);

        let fen_after = "5rk1/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 1 2";
        test_make_undo(fen_before, castle_move, fen_after);
    }

    #[test]
    fn test_xfen_black_queenside_castle_k_e8_r_c8() {
        let fen_before = "2r1k3/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b q - 0 1";

        let castle_move = Move::new(Square::E8, Square::C8, MoveFlag::QueenCastle);

        let fen_after = "2kr4/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 1 2";
        test_make_undo(fen_before, castle_move, fen_after);
    }

    #[test]
    fn test_xfen_sp4_white_kingside_castle() {
        let fen_before = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/R4KR1 w KQ - 0 1";
        let castle_move = Move::new(Square::F1, Square::G1, MoveFlag::KingCastle);
        let fen_after = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/R4RK1 b - - 1 1";
        test_make_undo(fen_before, castle_move, fen_after);
    }

    #[test]
    fn test_xfen_sp4_white_queenside_castle() {
        let fen_before = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/R4KR1 w KQ - 0 1";
        let castle_move = Move::new(Square::F1, Square::C1, MoveFlag::QueenCastle);
        let fen_after = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/2KR2R1 b - - 1 1";
        test_make_undo(fen_before, castle_move, fen_after);
    }

    #[test]
    fn test_xfen_rights_removal_king_move() {
        let fen_before = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQ2KR w KQ - 0 1";
        let king_move = Move::new(Square::G1, Square::F1, MoveFlag::QuietMove);
        let fen_after = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQ1K1R b - - 1 1";
        test_make_undo(fen_before, king_move, fen_after);
    }

    #[test]
    fn test_xfen_rights_removal_h_rook_move() {
        let fen_before = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKB1R w KQ - 0 1";

        let rook_move = Move::new(Square::H1, Square::G1, MoveFlag::QuietMove);
        let fen_after = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBR1 b Q - 1 1";
        test_make_undo(fen_before, rook_move, fen_after);
    }

    #[test]
    fn test_xfen_custom_position() {
        let fen_before = "rn2k1r1/ppp1pp1p/3p2p1/5bn1/P7/2N2B2/1PPPPP2/2BNK1RR w Gkq - 4 11";
        let fen_after = "rn2k1r1/ppp1pp1p/3p2p1/5bn1/P6R/2N2B2/1PPPPP2/2BNK1R1 b Kkq - 5 11";
        let rook_move = Move::new(Square::H1, Square::H4, MoveFlag::QuietMove);
        test_make_undo(fen_before, rook_move, fen_after);
    }

    fn assert_repetitions(board: &Board, expected: i8) {
        assert_eq!(board.state.repetitions, expected);
    }

    #[test]
    fn test_three_fold_repetition() {
        let mut board = board_from_fen(START_FEN);

        let nf3 = Move::new(Square::G1, Square::F3, MoveFlag::QuietMove);
        let nc6 = Move::new(Square::B8, Square::C6, MoveFlag::QuietMove);
        let ng1 = Move::new(Square::F3, Square::G1, MoveFlag::QuietMove);
        let nb8 = Move::new(Square::C6, Square::B8, MoveFlag::QuietMove);
        let nd4 = Move::new(Square::F3, Square::D4, MoveFlag::QuietMove);
        let ndf3 = Move::new(Square::D4, Square::F3, MoveFlag::QuietMove);

        board.make_move(nf3);
        board.make_move(nc6);
        assert_repetitions(&board, 0);
        board.make_move(ng1);
        board.make_move(nb8);
        assert_repetitions(&board, 3);
        board.make_move(nf3);
        board.make_move(nc6);
        assert_repetitions(&board, 3);
        board.make_move(ng1);
        board.make_move(nb8);
        assert_repetitions(&board, -3);
        board.make_move(nf3);
        board.make_move(nc6);
        assert_repetitions(&board, -3);
        board.make_move(nd4);
        board.make_move(nb8);
        assert_repetitions(&board, 0);
        board.make_move(ndf3);
        board.make_move(nc6);
        assert_repetitions(&board, -3);
    }
}
