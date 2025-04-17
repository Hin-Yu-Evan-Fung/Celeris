use super::Board;
use crate::core::*;
use crate::movegen::lookup::castling_rights;

impl Board {
    /// # Add Piece
    ///
    /// - Adds a piece to the board at the specified square
    /// - Updates the piece and occupied bitboards
    ///
    /// ## Arguments
    ///
    /// * `piece` - The piece to add
    /// * `square` - The square to add the piece to
    pub fn add_piece(&mut self, piece: Piece, square: Square) {
        // Put piece in the board
        self.board[square as usize] = Some(piece);
        // Update piece bitboards
        self.pieces[piece.pt() as usize].set(square);
        self.occupied[piece.colour() as usize].set(square);
    }

    /// # Remove Piece
    ///
    /// - Removes a piece from the board at the specified square
    /// - Updates the piece and occupied bitboards
    ///
    /// ## Arguments
    ///
    /// * `square` - The square to remove the piece from
    pub fn remove_piece(&mut self, square: Square) {
        // Get the piece to remove
        let piece = self.board[square as usize].unwrap();
        // Remove piece from the board
        self.board[square as usize] = None;
        // Update piece bitboards
        self.pieces[piece.pt() as usize].clear(square);
        self.occupied[piece.colour() as usize].clear(square);
    }

    /// # Move Piece
    ///
    /// - Streamlines the piece moving process
    /// - Moves a piece from one square to another
    /// - Updates the piece and occupied bitboards
    ///
    /// ## Arguments
    ///
    /// * `from` - The square to move the piece from
    /// * `to` - The square to move the piece to
    pub fn move_piece(&mut self, from: Square, to: Square) {
        // Get the piece to move
        let piece = self.board[from as usize].unwrap();
        // Remove piece from the board
        self.board[from as usize] = None;
        // Put piece in the board
        self.board[to as usize] = Some(piece);

        let from_to_bb = from.bb() | to.bb();
        // Update piece bitboards
        self.pieces[piece.pt() as usize] ^= from_to_bb;
        // Update occupied bitboards
        self.occupied[piece.colour() as usize] ^= from_to_bb;
    }

    // Updates enpassant squares and the hash key
    fn set_ep(&mut self, from: Square) {
        let us = self.side_to_move;
        // Set Enpassant Square
        self.state.enpassant = from.add(us.forward()).ok();
        // Toggle enpassant key (There must be an enpassant square)
        self.state
            .key
            .toggle_ep(self.state.enpassant.unwrap().file());
    }

    // Rook from square
    fn rook_from(&self, king_side: bool) -> Square {
        let us = self.side_to_move;
        match king_side {
            true => Square::H1.relative(us),
            false => Square::A1.relative(us),
        }
    }

    // Rook to square
    fn rook_to(&self, king_side: bool) -> Square {
        let us = self.side_to_move;
        match king_side {
            true => Square::F1.relative(us),
            false => Square::D1.relative(us),
        }
    }

    // Castling
    fn castle(&mut self, king_side: bool) {
        let us = self.side_to_move;
        let piece = Piece::from_parts(us, PieceType::Rook);

        // Get the source and destination squares of the rook, given the side to move
        let rook_from = self.rook_from(king_side);
        let rook_to = self.rook_to(king_side);

        // Move the rook
        self.move_piece(rook_from, rook_to);

        // Update hash key
        self.state.key.toggle_piece(piece, rook_from);
        // Update hash key
        self.state.key.toggle_piece(piece, rook_to);

        // Disclaimer: The King will be moved as part of the main make move function
    }

    // Reverse Castling
    fn rev_castle(&mut self, king_side: bool) {
        // Get the source and destination squares of the rook, given the side to move
        let rook_from = self.rook_from(king_side);
        let rook_to = self.rook_to(king_side);

        // Move the rook
        self.move_piece(rook_to, rook_from);

        // Disclaimer: The King will be moved as part of the main make move function
    }

    // Update castle rights
    fn update_castle(&mut self, from: Square, to: Square) {
        // Remove previous castling rights from hash key
        self.state.key.toggle_castle(self.state.castle);
        // Update castling rights
        self.state
            .castle
            .mask(castling_rights(from) & castling_rights(to));
        // Update hash key
        self.state.key.toggle_castle(self.state.castle);
    }

    /// # Make Move
    pub fn make_move(&mut self, move_: Move) {
        // Cache the current state (Now previous state)
        self.store_state();
        // Increment the half move counter
        self.half_moves += 1;

        // Initialise variables
        let from = move_.from();
        let to = move_.to();
        let us = self.side_to_move;
        let them = !us;
        let piece = self.on(from).unwrap();
        let flag = move_.flag();

        // Assume the move is not a capture or a pawn move first, because if it is the fifty_move counter is reset anyways
        self.state.fifty_move += 1;

        // Toggle enpassant key (remove enpassant) and remove enpassant square
        self.state.enpassant.map(|sq| {
            self.state.key.toggle_ep(sq.file());
            self.state.enpassant = None;
        });

        // Match move flag
        match flag {
            MoveFlag::QuietMove => {
                if piece.pt() == PieceType::Pawn {
                    // Reset fifty move counter since there is a pawn move
                    self.state.fifty_move = 0;
                }
                // Move the piece
                self.move_piece(from, to);
                // Update hash key
                self.state.key.toggle_piece(piece, from);
                // Update hash key
                self.state.key.toggle_piece(piece, to);
                // Toggle castling rights
                self.update_castle(from, to);
            }

            MoveFlag::DoublePawnPush => {
                // Reset fifty move counter since there is a pawn move
                self.state.fifty_move = 0;
                // Update enpassant square and keys
                self.set_ep(from);
                // Move the piece
                self.move_piece(from, to);
                // Update hash key
                self.state.key.toggle_piece(piece, from);
                // Update hash key
                self.state.key.toggle_piece(piece, to);
            }

            MoveFlag::KingCastle | MoveFlag::QueenCastle => {
                // Castle the rooks
                self.castle(flag == MoveFlag::KingCastle);
                // Move the king
                self.move_piece(from, to);
                // Update hash key
                self.state.key.toggle_piece(piece, from);
                // Update hash key
                self.state.key.toggle_piece(piece, to);
                // Toggle castling rights
                self.update_castle(from, to);
            }

            MoveFlag::Capture => {
                // Reset fifty move counter since there is a capture
                self.state.fifty_move = 0;
                // Store captured piece
                self.state.captured = self.on(to);
                // Remove the captured piece
                self.remove_piece(to);
                // Update hash key
                self.state
                    .key
                    .toggle_piece(self.state.captured.unwrap(), to);
                // Move the piece
                self.move_piece(from, to);
                // Update hash key
                self.state.key.toggle_piece(piece, from);
                // Update hash key
                self.state.key.toggle_piece(piece, to);
                // Toggle castling rights
                self.update_castle(from, to);
            }

            MoveFlag::EPCapture => {
                // Reset fifty move counter since there is a capture
                self.state.fifty_move = 0;
                // Calculate the square of the pawn that is captured
                let cap_sq = to.add(-us.forward()).unwrap();
                // Store captured piece
                self.state.captured = Some(Piece::from_parts(them, PieceType::Pawn));
                // Remove pawn that is captured
                self.remove_piece(cap_sq);
                // Update hash key
                self.state
                    .key
                    .toggle_piece(self.state.captured.unwrap(), cap_sq);
                // Move the piece
                self.move_piece(from, to);
                // Update hash key
                self.state.key.toggle_piece(piece, from);
                // Update hash key
                self.state.key.toggle_piece(piece, to);
            }

            MoveFlag::KnightPromo
            | MoveFlag::BishopPromo
            | MoveFlag::RookPromo
            | MoveFlag::QueenPromo => {
                // Reset fifty move counter since there is a pawn move
                self.state.fifty_move = 0;
                // Get promotion piece
                let promo_piece = Piece::from_parts(us, move_.promotion_pt());
                // Remove the current piece
                self.remove_piece(from);
                // Update hash key
                self.state.key.toggle_piece(piece, from);
                // Place down promotion piece
                self.add_piece(promo_piece, to);
                // Update hash key
                self.state.key.toggle_piece(promo_piece, to);
            }

            MoveFlag::KnightPromoCapture
            | MoveFlag::BishopPromoCapture
            | MoveFlag::RookPromoCapture
            | MoveFlag::QueenPromoCapture => {
                // Reset fifty move counter since there is a pawn move
                self.state.fifty_move = 0;
                // Store captured piece
                self.state.captured = self.on(to);
                // Remove the captured piece
                self.remove_piece(to);
                // Update hash key
                self.state
                    .key
                    .toggle_piece(self.state.captured.unwrap(), to);
                // Get promotion piece
                let promo_piece = Piece::from_parts(us, move_.promotion_pt());
                // Remove the current piece
                self.remove_piece(from);
                // Update hash key
                self.state.key.toggle_piece(piece, from);
                // Place down promotion piece
                self.add_piece(promo_piece, to);
                // Update hash key
                self.state.key.toggle_piece(promo_piece, to);
            }
        }

        // Toggle side to move
        self.side_to_move = !self.side_to_move;
        // Update hash key
        self.state.key.toggle_colour();
    }

    /// # Undo Move
    pub fn undo_move(&mut self, move_: Move) {
        // Toggle side to move
        self.side_to_move = !self.side_to_move;

        // Increment the half move counter
        self.half_moves -= 1;

        // Initialise variables
        let from = move_.from();
        let to = move_.to();
        let us = self.side_to_move;
        let captured = self.state.captured;
        let flag = move_.flag();

        // Restore previous state
        self.restore_state();

        // Reverse move
        match flag {
            MoveFlag::QuietMove | MoveFlag::DoublePawnPush => {
                // Move the piece
                self.move_piece(to, from);
                // Enpassant removed when state is restored
            }
            MoveFlag::Capture => {
                // Move the piece
                self.move_piece(to, from);
                // Place down captured piece
                self.add_piece(captured.unwrap(), to);
            }
            MoveFlag::EPCapture => {
                // Move the piece;
                self.move_piece(to, from);
                // Place down captured pawn
                self.add_piece(captured.unwrap(), to.add(-us.forward()).unwrap());
            }
            MoveFlag::KingCastle | MoveFlag::QueenCastle => {
                // Reverse castle the rooks
                self.rev_castle(flag == MoveFlag::KingCastle);
                // Move the king
                self.move_piece(to, from);
                // Castling rights are restored when state is restored
            }
            MoveFlag::KnightPromo
            | MoveFlag::BishopPromo
            | MoveFlag::RookPromo
            | MoveFlag::QueenPromo => {
                // Remove promoted piece
                self.remove_piece(to);
                // Add the pawn back to the from square
                self.add_piece(Piece::from_parts(us, PieceType::Pawn), from);
            }
            MoveFlag::KnightPromoCapture
            | MoveFlag::BishopPromoCapture
            | MoveFlag::RookPromoCapture
            | MoveFlag::QueenPromoCapture => {
                // Remove promoted piece
                self.remove_piece(to);
                // Add the captured pawn back to the from square
                self.add_piece(captured.unwrap(), to);
                // Add the pawn back to the from square
                self.add_piece(Piece::from_parts(us, PieceType::Pawn), from);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import Board and its methods
    use crate::board::fen::*; // Import FEN constants and Board::from_fen
    use crate::core::*; // Import Piece, Square, Move, MoveFlag, etc.

    // Helper function to create a board and calculate its key consistently
    fn board_from_fen(fen: &str) -> Board {
        let mut board = Board::from_fen(fen).expect("Test FEN should be valid");
        // Ensure the key stored after parsing matches a fresh calculation
        let calculated_key = board.calc_key();
        assert_eq!(
            board.state().key,
            calculated_key,
            "Key mismatch after initial FEN parse for: {}",
            fen
        );
        board
    }

    // Helper function to perform make/undo and check board state + key consistency
    fn test_make_undo(fen_before: &str, move_to_test: Move, fen_after: &str) {
        let mut board = board_from_fen(fen_before);
        let key_before = board.state().key;

        // --- Make Move ---
        board.make_move(move_to_test);
        let key_after_make = board.state().key;

        // Assert board state after make_move
        assert_eq!(
            board.fen(),
            fen_after,
            "FEN mismatch after make_move for '{}'",
            move_to_test
        );
        // Assert key is different after make_move (unless it's a null move, which we aren't testing here)
        assert_ne!(
            key_before, key_after_make,
            "Key should change after make_move for '{}'",
            move_to_test
        );

        // --- Undo Move ---
        board.undo_move(move_to_test);
        let key_after_undo = board.state().key;

        // Assert board state is restored after undo_move
        assert_eq!(
            board.fen(),
            fen_before,
            "FEN mismatch after undo_move for '{}'",
            move_to_test
        );
        // Assert key is restored exactly
        assert_eq!(
            key_after_undo, key_before,
            "Key mismatch after undo_move for '{}'",
            move_to_test
        );

        // --- Optional: Verify key calculation consistency after undo ---
        // This helps catch if undo_move modified pieces correctly but restore_state failed somehow
        let calculated_key_after_undo = board.calc_key();
        assert_eq!(
            calculated_key_after_undo, key_before,
            "Recalculated key mismatch after undo_move for '{}'",
            move_to_test
        );
    }

    #[test]
    fn test_quiet_pawn_move() {
        test_make_undo(
            START_FEN,
            Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush), // e2e4
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
        );
    }

    #[test]
    fn test_quiet_knight_move() {
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1", // After 1. e4
            Move::new(Square::G8, Square::F6, MoveFlag::QuietMove),        // ...Nf6
            "rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2", // Corrected FEN
        );
    }

    #[test]
    fn test_capture() {
        test_make_undo(
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2", // After 1.e4 d5
            Move::new(Square::E4, Square::D5, MoveFlag::Capture),            // exd5
            "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2",
        );
    }

    #[test]
    fn test_en_passant_capture() {
        // Setup: 1. e4 (no move) 2. e5 d5 3. exd6 e.p.
        let fen_before_ep = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"; // White pawn e5, black pawn d5, EP on d6 possible
        test_make_undo(
            fen_before_ep,
            Move::new(Square::E5, Square::D6, MoveFlag::EPCapture), // White captures EP
            "rnbqkbnr/ppp1pppp/3P4/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3", // Black d5 pawn gone, white pawn on d6
        );
    }

    #[test]
    fn test_black_en_passant_capture() {
        // Setup: Starting Position
        let fen_before_ep = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"; // Start
        let mut board = board_from_fen(fen_before_ep);
        board.make_move(Move::new(Square::D2, Square::D4, MoveFlag::DoublePawnPush)); // 1. d4
        board.make_move(Move::new(Square::F7, Square::F5, MoveFlag::QuietMove)); // 1... f5
        board.make_move(Move::new(Square::D4, Square::D5, MoveFlag::QuietMove)); // 2. d5
        board.make_move(Move::new(Square::E7, Square::E5, MoveFlag::DoublePawnPush)); // 2... e5 (EP possible on e6)

        let fen_before_black_ep = board.fen();
        assert_eq!(
            fen_before_black_ep,
            "rnbqkbnr/pppp2pp/8/3Ppp2/8/8/PPP1PPPP/RNBQKBNR w KQkq e6 0 3"
        );

        test_make_undo(
            &fen_before_black_ep,
            Move::new(Square::D5, Square::E6, MoveFlag::EPCapture), // White captures EP d5xe6
            "rnbqkbnr/pppp2pp/4P3/5p2/8/8/PPP1PPPP/RNBQKBNR b KQkq - 0 3", // Black e5 pawn gone, white pawn on e6
        );
    }

    #[test]
    fn test_white_kingside_castle() {
        let fen_before_castle = "rnbq1bnr/pppppkpp/8/8/8/8/PPPPPPPP/RNBQK2R w KQ - 0 5"; // King/Rook moved, Black K moved
        test_make_undo(
            fen_before_castle,
            Move::new(Square::E1, Square::G1, MoveFlag::KingCastle), // O-O
            "rnbq1bnr/pppppkpp/8/8/8/8/PPPPPPPP/RNBQ1RK1 b - - 1 5", // Rook F1, King G1, WK rights gone
        );
    }

    #[test]
    fn test_black_queenside_castle() {
        let fen_before_castle = "r3kbnr/p1pp1ppp/bpn1p3/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 7"; // Ready for O-O-O
        test_make_undo(
            fen_before_castle,
            Move::new(Square::E8, Square::C8, MoveFlag::QueenCastle), // O-O-O
            "2kr1bnr/p1pp1ppp/bpn1p3/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 1 8", // Rook D8, King C8, BQ rights gone
        );
    }

    #[test]
    fn test_promotion_quiet() {
        let fen_before_promo = "r1bqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6"; // White pawn on b7
        test_make_undo(
            fen_before_promo,
            Move::new_promotion(Square::B7, Square::B8, PieceType::Queen, false), // b7b8=Q
            "rQbqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 6", // White Queen on b8
        );
    }

    #[test]
    fn test_promotion_capture() {
        let fen_before_promo_cap = "r1bqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6"; // White pawn b7, Black rook a8
        test_make_undo(
            fen_before_promo_cap,
            Move::new_promotion(Square::B7, Square::A8, PieceType::Knight, true), // b7xa8=N
            "N1bqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 6", // White Knight on a8
        );
    }

    #[test]
    fn test_castling_rights_king_move() {
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1", // Start without E2 Pawn
            Move::new(Square::E1, Square::E2, MoveFlag::QuietMove),     // Ke2
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPKPPP/RNBQ1BNR b kq - 1 1",
        );
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR w KQkq - 0 1",
            Move::new(Square::E1, Square::D1, MoveFlag::QuietMove), // Ke2
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBK1BNR b kq - 1 1", // White loses KQ rights
        );
    }

    #[test]
    fn test_castling_rights_rook_move() {
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 1", // Start without A2 Pawn
            Move::new(Square::A1, Square::A2, MoveFlag::QuietMove),     // Ra2
            "rnbqkbnr/pppppppp/8/8/8/8/RPPPPPPP/1NBQKBNR b Kkq - 1 1",  // White loses Q rights
        );
        test_make_undo(
            "rnbqkbnr/ppppppp1/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1", // Start without A2 Pawn
            Move::new(Square::H8, Square::H6, MoveFlag::QuietMove),     // Rh7
            "rnbqkbn1/ppppppp1/7r/8/8/8/PPPPPPPP/RNBQKBNR w KQq - 1 2", // Black loses k rights
        );
    }

    #[test]
    fn test_castling_rights_rook_capture() {
        // Setup: Black rook leaves A8
        let fen_before_capture = "rnbqkbnr/1ppppppp/8/p7/P7/8/1PPPPPPP/RNBQKBNR w KQkq - 0 3";
        let mut board = Board::from_fen(fen_before_capture).expect("Test FEN should be valid");
        board.make_move(Move::new(Square::B2, Square::B4, MoveFlag::QuietMove)); // b4
        board.make_move(Move::new(Square::A5, Square::B4, MoveFlag::Capture)); // b4
        board.make_move(Move::new(Square::A8, Square::A4, MoveFlag::Capture)); // Ra6
        // Simpler: White Knight captures Black Rook on a8
        let fen_before_capture = "rnbqkbnr/pppppppp/1N6/8/8/8/PPPPPPPP/R1BQKBNR w KQkq - 0 1";
        let mut board_simple =
            Board::from_fen(fen_before_capture).expect("Test FEN should be valid");
        board_simple.make_move(Move::new(Square::B6, Square::A8, MoveFlag::Capture)); // Nxa8

        let fen_after_capture = board_simple.fen();
        assert_eq!(
            fen_after_capture,
            "Nnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R1BQKBNR b KQk - 0 1"
        ); // Black loses q rights

        test_make_undo(
            "rnbqkbnr/pppppppp/1N6/8/8/8/PPPPPPPP/R1BQKBNR w KQkq - 0 1", // State before capture
            Move::new(Square::B6, Square::A8, MoveFlag::Capture),         // Nxa8
            "Nnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R1BQKBNR b KQk - 0 1", // State after capture, Black loses q right
        );
    }

    #[test]
    fn test_fifty_move_counter() {
        // Test that the fifty-move counter resets on pawn moves and captures
        let mut board = board_from_fen(START_FEN);
        assert_eq!(board.state().fifty_move, 0); // Initial state

        // Quiet pawn move
        board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
        assert_eq!(board.state().fifty_move, 0); // Reset on pawn move
        board.undo_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));

        // Capture
        board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::D7, Square::D5, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::E4, Square::D5, MoveFlag::Capture));
        assert_eq!(board.state().fifty_move, 0); // Reset on capture
        board.undo_move(Move::new(Square::E4, Square::D5, MoveFlag::Capture));
        board.undo_move(Move::new(Square::D7, Square::D5, MoveFlag::DoublePawnPush));
        board.undo_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));

        // Quiet knight move
        board.make_move(Move::new(Square::G1, Square::F3, MoveFlag::QuietMove));
        assert_eq!(board.state().fifty_move, 1); // Increment on non-pawn, non-capture
        board.undo_move(Move::new(Square::G1, Square::F3, MoveFlag::QuietMove));

        // Quiet king move
        board.make_move(Move::new(Square::E1, Square::E2, MoveFlag::QuietMove));
        assert_eq!(board.state().fifty_move, 1); // Increment on non-pawn, non-capture
    }
}
