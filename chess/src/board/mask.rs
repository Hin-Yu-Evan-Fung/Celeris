//! # Module: `board::mask`
//!
//! This module extends the `Board` struct with methods specifically focused on
//! calculating bitmasks relevant to attacks, checks, and pins. These masks are
//! crucial for efficient move generation, especially in determining legal moves
//! when the king is under threat. The calculated masks (`attacked`, `check_mask`,
//! `diag_pin`, `hv_pin`) are typically stored within the `BoardState` for quick
//! access during move generation, avoiding recalculation for every potential move.
//!
use super::Board;
use super::movegen::*;
use crate::core::*; // Assuming movegen functions like pawn_attack_span, leaper_attack, attacks,us,  pin_bb are here


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

/******************************************\
|==========================================|
|               Update Masks               |
|==========================================|
\******************************************/

impl Board {
    /// Helper function to get a bitboard of bishops and queens for a given colour.
    ///
    /// Combines the bitboards for bishops and queens of the specified `col`.
    /// Used internally for calculating slider attacks and pins.
    #[inline]
    pub(crate) fn bishop_queen_bb(&self, col: Colour) -> Bitboard {
        self.piece_bb(col, PieceType::Bishop) | self.piece_bb(col, PieceType::Queen)
    }

    /// Helper function to get a bitboard of rooks and queens for a given colour.
    ///
    /// Combines the bitboards for rooks and queens of the specified `col`.
    /// Used internally for calculating slider attacks and pins.
    #[inline]
    pub(crate) fn rook_queen_bb(&self, col: Colour) -> Bitboard {
        self.piece_bb(col, PieceType::Rook) | self.piece_bb(col, PieceType::Queen)
    }

    /// Helper function to get the square of the king for a given colour.
    ///
    /// Assumes the king exists on the board for the given colour.
    /// Uses `lsb_unchecked` for performance, relying on the invariant that
    /// a king must be present in a valid chess position.
    ///
    /// # Arguments
    ///
    /// * `col`: The `Colour` of the king to find.
    ///
    /// # Returns
    ///
    /// The `Square` where the specified king is located.
    ///
    /// # Panics
    ///
    /// Panics in debug mode if no king is found for the given colour.
    /// Behavior is undefined in release mode if no king is found (due to `lsb_unchecked`).
    #[inline]
    pub fn ksq(&self, col: Colour) -> Square {
        // This assertion is crucial for safety when using lsb_unchecked
        debug_assert!(
            !self.piece_bb(col, PieceType::King).is_empty(),
            "King must exist for colour {:?}",
            col
        );

        // Okay to use unchecked because a valid board state guarantees a king.
        self.piece_bb(col, PieceType::King).lsb_unchecked()
    }

    /// Gets the pre-calculated bitboard of squares attacked by the opponent.
    ///
    /// This mask represents all squares attacked by the side *not* currently to move.
    /// It's stored in `BoardState` and updated by `update_masks`.
    #[inline]
    pub const fn attacked(&self) -> Bitboard {
        self.state.attacked
    }

    /// Gets the pre-calculated check mask.
    ///
    /// This mask indicates how a check must be handled.
    /// - `Bitboard::FULL`: The king is not in check.
    /// - `Bitboard::EMPTY`: The king is in double check (only king moves are legal).
    /// - Otherwise: The king is in single check. The mask contains the squares
    ///   where a piece can move to block the check or capture the checking piece.
    ///   For knight/pawn checks, this is just the checker's square. For slider checks,
    ///   it's the checker's square plus the squares between the checker and the king.
    ///
    /// It's stored in `BoardState` and updated by `update_masks`.
    #[inline]
    pub const fn check_mask(&self) -> Bitboard {
        self.state.check_mask
    }

    /// Gets the pre-calculated bitboard representing diagonal pins.
    ///
    /// Contains the squares along diagonal rays originating from the king,
    /// extending towards an enemy bishop or queen, but only if a friendly piece
    /// lies on that ray between the king and the enemy slider. The bits represent
    /// the valid squares a pinned piece can move to *along the pin ray*.
    /// It's stored in `BoardState` and updated by `update_masks`.
    #[inline]
    pub const fn diag_pin(&self) -> Bitboard {
        self.state.diag_pin
    }

    /// Gets the pre-calculated bitboard representing horizontal/vertical pins.
    ///
    /// Contains the squares along horizontal or vertical rays originating from the king,
    /// extending towards an enemy rook or queen, but only if a friendly piece
    /// lies on that ray between the king and the enemy slider. The bits represent
    /// the valid squares a pinned piece can move to *along the pin ray*.
    /// It's stored in `BoardState` and updated by `update_masks`.
    #[inline]
    pub const fn hv_pin(&self) -> Bitboard {
        self.state.hv_pin
    }

    /// Gets whether the enpassant pawn blocks a check
    #[inline]
    pub const fn ep_pin(&self) -> bool {
        self.state.ep_pin
    }

    /// Calculates a bitboard of all squares attacked by the opponent.
    ///
    /// This function determines which squares are under attack by the side
    /// whose turn it is *not*. It iterates through all opponent pieces
    /// (pawns, knights, bishops, rooks, queens, king) and calculates their attacks.
    ///
    /// A crucial detail is how slider attacks (bishop, rook, queen) are handled:
    /// the occupancy bitboard used for calculating these attacks *excludes* the
    /// friendly king (`us`). This allows sliders to "see through" the friendly
    /// king's square, correctly identifying attacks that would occur if the king
    /// moved away, which is essential for check detection and validating king moves.
    ///
    /// # Returns
    ///
    /// A `Bitboard` where set bits represent squares attacked by the opponent
    /// (`!self.side_to_move`).
    // 8 Branches
    #[inline]
    fn calc_attacked_bb(&self) -> Bitboard {
        let us = self.stm;
        let them = !us;
        // Occupancy excluding the friendly king for slider attack calculations.
        // This allows sliders to "see through" the friendly king's square.
        let occ = self.all_occupied_bb() ^ self.piece_bb(us, PieceType::King);

        // Start with pawn attacks
        let mut threatened = Bitboard::pawn_attacks(them, self.piece_bb(them, PieceType::Pawn));

        // Add knight attacks
        let knight_bb = self.piece_bb(them, PieceType::Knight);
        knight_bb.for_each(|sq| {
            // threatened |= leaper_attack(PieceType::Knight, sq);
            threatened |= knight_attack(sq);
        });

        // Add bishop/queen diagonal attacks
        let bishop_queen_bb = self.bishop_queen_bb(them);
        bishop_queen_bb.for_each(|sq| {
            threatened |= bishop_attacks(sq, occ);
        });

        // Add rook/queen horizontal/vertical attacks
        let rook_queen_bb = self.rook_queen_bb(them);
        rook_queen_bb.for_each(|sq| {
            threatened |= rook_attacks(sq, occ);
        });

        threatened |= attacks(us, PieceType::King, self.ksq(them), occ); // Add king

        threatened
    }

    /// Calculates the bitboards representing pinned pieces along diagonals and HV lines.
    ///
    /// A piece is pinned if it lies on a straight line (rank, file, or diagonal)
    /// between its own king and an attacking slider piece (rook, bishop, or queen)
    /// of the opponent.
    ///
    /// This function identifies such pinned pieces for the side currently to move (`us`).
    /// It returns two bitboards: one for diagonal pins (caused by bishops/queens)
    /// and one for horizontal/vertical pins (caused by rooks/queens).
    ///
    /// The bits set in the returned `Bitboard`s represent the squares *along the pin ray*
    /// between the king and the pinner (inclusive of the pinner's square, exclusive of the king's).
    /// A pinned piece can only legally move to squares within this pin mask.
    ///
    /// # Returns
    ///
    /// A tuple `(diag_pin, hv_pin)`:
    /// * `diag_pin`: `Bitboard` representing squares involved in diagonal pins.
    /// * `hv_pin`: `Bitboard` representing squares involved in horizontal/vertical pins.
    /// * 'ep_pin': Whether the enpassant pawn is a blocker for a check
    // 5 branches
    #[inline]
    #[rustfmt::skip]
    fn calc_pin_mask(&self) -> (Bitboard, Bitboard) {
        let us = self.stm;
        let them = !us;
        let ksq = self.ksq(us); // King square of the side to move

        let all_occ = self.all_occupied_bb();
        let our_occ = self.occupied_bb(us);
        let them_occ = self.occupied_bb(them);

        let mut diag_pin = Bitboard::EMPTY;
        let mut hv_pin = Bitboard::EMPTY;

        // 1. Probe rays are like rays that radiate from the kings position to find potential pinned pieces and checkers
        let probe_rays = queen_attacks(ksq, all_occ);
        // 2. Find our pieces that are potentially pinned
        let potential_pinned = probe_rays & our_occ;
        // 3. Find their pieces that are potential checkers
        let potential_checkers = probe_rays & them_occ;
        // 4. Remove potential pinned pieces to see if there are slider pinners beyond them
        let occ = all_occ ^ potential_pinned;
        // 5. Remove potential diagonally pinned enpassant target pawn

        // 6. Find the diagonal pinners (Bishop/Queen)
        let diag_pinners = bishop_attacks(ksq, occ)
            & self.bishop_queen_bb(them)
            & !potential_checkers;
        diag_pinners.for_each(|sq| diag_pin |= pin_bb(ksq, sq));

        // 7. Find the horizontal/vertical pinners (Rook/Queen)
        let hv_pinners = rook_attacks(ksq, occ)
            & self.rook_queen_bb(them) 
            & !potential_checkers;
        hv_pinners.for_each(|sq| hv_pin |= pin_bb(ksq, sq));

        (diag_pin, hv_pin)
    }

    /// Calculates whether the en passant target pawn is pinned to the king, preventing the en passant capture.
    ///
    /// This function checks if the pawn that could be captured en passant is pinned to its king by an enemy slider.
    /// If the en passant capture would expose the king to check, the capture is illegal.
    ///
    /// # Arguments
    ///
    /// * `ep_target`: The `Square` of the pawn that could be captured en passant.
    /// * `attackers`: A `Bitboard` of the pawns that can perform the enpassant capture.
    ///
    /// # Returns
    ///
    /// `true` if the en passant target pawn is pinned and the capture would be illegal, `false` otherwise.
    ///
    /// # Details
    ///
    /// 1. **Identify Potential Checkers:** It first identifies potential checkers (enemy sliders) that could attack the king.
    /// 2. **Remove En Passant Target:** It temporarily removes the en passant target pawn from the board to see if any sliders are revealed.
    /// 3. **Check for Diagonal Pinners:** It checks for diagonal pinners (enemy bishops/queens) that could pin the en passant target pawn.
    /// 4. **Check for Horizontal Pinners:** If there is only one attacker, it checks for horizontal pinners (enemy rooks/queens) that could pin the en passant target pawn.
    /// 5. **Return Result:** Returns `true` if a pin is found, `false` otherwise.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use chess::Board;
    /// use chess::core::*;
    ///
    /// let mut board = Board::from_fen("2k5/8/8/K2pP2r/8/8/8/8 w - d6 0 1").unwrap();
    /// board.update_masks();
    ///
    /// assert_eq!(board.ep_pin(), true);
    /// ```
    #[inline]
    fn calc_ep_pin(&self, ep_target: Square, attackers: Bitboard) -> bool {
        let us = self.stm;
        let them = !us;
        let ksq = self.ksq(us); // King square of the side to move
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

    /// Calculates the check mask based on attackers targeting the king of the side to move.
    ///
    /// This function identifies all pieces of the opponent (`them`) that are currently
    /// attacking the king (`us`) square (`ksq`). Based on the number and type of
    /// attackers, it determines the `check_mask`.
    ///
    /// The `check_mask` defines the set of squares relevant for resolving the check:
    /// - **No Check:** Returns `Bitboard::FULL`. Any move is potentially valid regarding check.
    /// - **Single Check (Pawn/Knight):** Returns a `Bitboard` with only the checker's square set.
    ///   The check can only be resolved by capturing the checker (or moving the king).
    /// - **Single Check (Slider - Bishop/Rook/Queen):** Returns a `Bitboard` representing the ray
    ///   between the king and the checker, including the checker's square. The check can be
    ///   resolved by capturing the checker or blocking the attack along this ray (or moving the king).
    /// - **Double Check (or more):** Returns `Bitboard::EMPTY`. Only king moves are legal. Blocking
    ///   or capturing is impossible as it cannot resolve both checks simultaneously.
    ///
    /// This optimized approach calculates all potential checker types first, combines them,
    /// counts the total number of checkers, and then determines the mask, reducing branching.
    ///
    /// # Returns
    ///
    /// A `Bitboard` representing the check mask as described above.
    // 5 branches
    #[inline]
    fn calc_check_mask(&self) -> Bitboard {
        let us = self.stm;
        let ksq = self.ksq(us);
        let them = !us;
        let occ = self.all_occupied_bb(); // Use full occupancy for check detection

        // 1. Calculate bitboards of all potential checkers targeting the king square.
        //    Pawn checks require looking "backwards" from the king square.
        let pawn_checkers = pawn_attack(us, ksq) & self.piece_bb(them, PieceType::Pawn);
        // Knight checks are direct attacks on the king square.
        let knight_checkers = knight_attack(ksq) & self.piece_bb(them, PieceType::Knight);
        // Slider checks use attacks *from* the king square with full occupancy.
        let diag_checkers = bishop_attacks(ksq, occ) & self.bishop_queen_bb(them);
        let hv_checkers = rook_attacks(ksq, occ) & self.rook_queen_bb(them);

        // 2. Combine all checkers into a single bitboard.
        let all_checkers = pawn_checkers | knight_checkers | diag_checkers | hv_checkers;

        // 3. Count the number of checkers and determine the appropriate mask.
        if all_checkers.is_empty() {
            Bitboard::FULL
        } else if !all_checkers.more_than_one() {
            // Single check. Determine if it's a slider or non-slider.
            let checker_sq = all_checkers.lsb_unchecked(); // Safe because count is 1.

            // Check if the single checker is a pawn or knight.
            if (pawn_checkers | knight_checkers).contains(checker_sq) {
                // Pawn or Knight check: Mask is just the checker's square.
                // Must capture the checker (or move king).
                all_checkers
            } else {
                // Slider (Bishop, Rook, Queen) check: Mask is the ray between king and checker.
                // Must capture checker, block along the ray, or move king.
                pin_bb(ksq, checker_sq) // Includes checker_sq and squares between.
            }
        } else {
            Bitboard::EMPTY
        }
    }

    /// Recalculates and updates the attack, check, and pin masks stored in the `BoardState`.
    ///
    /// This method should be called after any change to the board position (e.g., after
    /// `make_move`) to ensure the stored masks accurately reflect the current state.
    /// These masks are then used for efficient move generation and validation.
    ///
    /// **Optimization:** If the king is in double check (`check_mask` is empty after calculation),
    /// the pin masks (`diag_pin`, `hv_pin`) are not calculated or are set to empty.
    /// This is because in a double check, only king moves are legal, so pinned pieces
    /// cannot move anyway, making the pin masks irrelevant for move generation in that specific state.
    pub(crate) fn update_masks(&mut self) {
        // Calculate squares attacked by the opponent.
        self.state.attacked = self.calc_attacked_bb();

        // Calculate the check mask for the current side to move.
        let check_mask = self.calc_check_mask();
        self.state.check_mask = check_mask;

        // Calculate pin masks only if not in double check.
        // In double check (check_mask == EMPTY), only king moves are legal,
        // so pins on other pieces are irrelevant for move generation.
        // If not in check (check_mask == FULL), we still need pins.
        if !check_mask.is_empty() {
            (self.state.diag_pin, self.state.hv_pin) = self.calc_pin_mask();

            if let Some(ep_target) = self.ep_target() {
                let our_pawns = self.piece_bb(self.stm, PieceType::Pawn);

                let ep_target_bb = ep_target.bb();

                // check if the enpassant pawn is capturable this turn
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
        // Initial position, white to move. Test squares attacked by black.
        let mut board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
        board.update_masks();
        let attacked_by_black = board.attacked();

        // Expected attacks: Rank 7 pawns attack rank 6, knights attack c6, f6, a6, h6
        let expected = Rank::Rank6.bb()
            | Rank::Rank7.bb()
            | Rank::Rank8.bb() ^ Square::A8.bb() ^ Square::H8.bb();

        assert_eq!(attacked_by_black, expected);
    }

    #[test]
    fn test_attacked_bb_sliders_blocked() {
        // White to move. Test squares attacked by black rook/bishop blocked by pawns.
        let mut board =
            Board::from_fen("r1b1kbnr/p1p1p1p1/8/8/8/8/P1P1P1P1/R1B1KBNR w KQkq - 0 1").unwrap();
        board.update_masks();

        let attacked_by_black = board.attacked();

        // Black rook a8 attacks a7. Black bishop c8 attacks b7, d7. etc.
        // Knights attack b6, d6, e6, g6.
        let expected = Bitboard(0x6efbbba0c0808080);

        assert_eq!(attacked_by_black, expected);
    }

    #[test]
    fn test_attacked_bb_attack_through_king() {
        let mut board = Board::from_fen("4rk2/8/8/8/8/8/8/4K3 w - - 0 1").unwrap();
        board.update_masks();

        let attacked_by_black = board.attacked();

        // Black rook attacks entire e-file except e1 itself.
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
        // White king e1, white rook e2, black rook e8. White to move. Rook e2 is pinned.
        let mut board = Board::from_fen("4rk2/8/8/8/8/8/4R3/4K3 w - - 0 1").unwrap();
        board.update_masks();
        assert_eq!(board.check_mask(), Bitboard::FULL);
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), pin_bb(Square::E1, Square::E8)); // Rook at e2 is pinned
    }

    #[test]
    fn test_pin_and_check_mask_diag_pin() {
        // White king e1, white bishop d2, black bishop a5. White to move. Bishop d2 is pinned.
        let mut board = Board::from_fen("5k2/8/8/b7/8/8/3B4/4K3 w - - 0 1").unwrap();
        board.update_masks();
        assert_eq!(board.check_mask(), Bitboard::FULL);
        assert_eq!(board.diag_pin(), pin_bb(Square::E1, Square::A5)); // Bishop at d2 is pinned
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_and_check_mask_single_slider_check() {
        // White king e1, black rook e8. White to move. In check.
        let mut board = Board::from_fen("4rk2/8/8/8/8/8/8/4K3 w - - 0 1").unwrap();
        board.update_masks();
        // Check mask should include the checker (e8) and squares between (e2-e7)
        let expected_check_mask = pin_bb(Square::E1, Square::E8);
        assert_eq!(board.check_mask(), expected_check_mask);
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        println!("{}", board.hv_pin());
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_and_check_mask_single_knight_check() {
        // White king e1, black knight d3. White to move. In check.
        let mut board = Board::from_fen("4k3/8/8/8/8/3n4/8/4K3 w - - 0 1").unwrap();
        board.update_masks();

        // Check mask should include only the checker (d3)
        assert_eq!(board.check_mask(), Square::D3.bb());
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);
    }

    #[test]
    fn test_pin_and_check_mask_double_check() {
        // White king e1, black rook e8, black bishop g3 (discovered check). White to move. In check.
        let board = Board::from_fen("4rk2/8/8/8/8/6b1/8/4K3 w - - 0 1").unwrap();
        // Pass in_check = true

        // Check mask should include the rook line + rook (e2-e8) AND the bishop (g3)
        assert_eq!(board.check_mask(), Bitboard::EMPTY);
        assert_eq!(board.diag_pin(), Bitboard::EMPTY); // No pins in double check
        assert_eq!(board.hv_pin(), Bitboard::EMPTY); // No pins in double check
    }

    #[test]
    fn test_pin_and_check_mask_pin_and_check() {
        // White king e1, white rook e2 (pinned by black rook e8), black knight checks from d3. White to move.
        let mut board = Board::from_fen("4rk2/8/8/8/8/3n4/4R3/4K3 w - - 0 1").unwrap();
        board.update_masks();

        // Check mask should contain only the knight checker
        assert_eq!(board.check_mask(), Square::D3.bb());
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        // The rook at e2 is still pinned
        assert_eq!(board.hv_pin(), Bitboard(0x1010101010101000));
    }

    #[test]
    fn test_mask_multiple_pins() {
        // White King A1, Black Rook C8 (pins N D2 diag), Black Bishop D5 (pins R D3 hv)
        // White to move. Knight D2 should be diagonally pinned, Rook D3 horizontally.
        let mut board = Board::from_fen("r6k/8/8/8/3b4/N7/1R6/K1B4q w - - 0 1").unwrap();
        board.update_masks();

        let ksq = Square::A1;

        // Note: pin_bb calculates the squares *between* king and pinner.
        // The pinned piece itself is on one of these squares.
        let expected_diag_pin = pin_bb(ksq, Square::D4); // Should contain D2
        let expected_hv_pin = pin_bb(ksq, Square::A8) | pin_bb(ksq, Square::H1); // Should contain D3

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
        // White King E1, Black Rook A8, White Rook E2, White Rook A1.
        // White to move. Rook E2 is NOT pinned by A8 because A1 is in the way.
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
        // Black King E8, White Pawn D7. Black to move.
        let mut board = Board::from_fen("4k3/3P4/8/8/8/8/8/4K3 b - - 0 1").unwrap();
        board.update_masks(); // Update masks for Black's turn

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
        // White King E1, Black Pawn D2. White to move.
        let mut board = Board::from_fen("4k3/8/8/8/8/8/3p4/4K3 w - - 0 1").unwrap();
        board.update_masks(); // Update masks for White's turn

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
        // White King E1, Black Queen B4. White to move.
        let mut board = Board::from_fen("4k3/8/8/8/1q6/8/8/4K3 w - - 0 1").unwrap();
        board.update_masks();

        let expected_check_mask = pin_bb(Square::E1, Square::B4); // Line B4-D2 + B4 itself
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
        // White King E1, Black Queen H1. White to move.
        let mut board = Board::from_fen("4k3/8/8/8/8/8/8/4K2q w - - 0 1").unwrap();
        board.update_masks();

        let expected_check_mask = pin_bb(Square::E1, Square::H1); // Line F1-G1 + H1 itself
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
        // White King E1, Black Rook E8, Black Knight D3. White to move. Double check.
        let mut board = Board::from_fen("4rk2/8/8/8/8/3n4/8/4K3 w - - 0 1").unwrap();
        board.update_masks();

        // In double check, check_mask should be EMPTY according to the new logic
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
        // Black king A8, White Queen B6, White King A6. Black to move. Stalemate.
        let mut board = Board::from_fen("k7/8/KQ6/8/8/8/8/8 b - - 0 1").unwrap();
        board.update_masks(); // Update masks for Black's turn

        assert_eq!(
            board.check_mask(),
            Bitboard::FULL,
            "Should not be in check (stalemate)"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);

        // Verify attacked squares around black king
        let attacked_by_white = board.attacked();
        assert!(attacked_by_white.contains(Square::A7)); // Attacked by King A6
        assert!(attacked_by_white.contains(Square::B7)); // Attacked by King A6 and Queen B6
        assert!(attacked_by_white.contains(Square::B8)); // Attacked by King A6 and Queen B6
    }

    #[test]
    fn test_mask_checkmate_position_back_rank() {
        // White King G1, Black King E8, White Rook H8. Black to move. Checkmate.
        let mut board = Board::from_fen("4k2R/8/8/8/8/8/8/6K1 b - - 0 1").unwrap();
        board.update_masks(); // Update masks for Black's turn

        let expected_check_mask = pin_bb(Square::E8, Square::H8); // Line F8-G8 + H8
        assert_eq!(
            board.check_mask(),
            expected_check_mask,
            "Check mask should be line from Rook H8"
        );
        assert_eq!(board.diag_pin(), Bitboard::EMPTY);
        assert_eq!(board.hv_pin(), Bitboard::EMPTY);

        // Verify attacked squares around black king
        let attacked_by_white = board.attacked();
        assert!(attacked_by_white.contains(Square::E8));
        assert!(attacked_by_white.contains(Square::F8)); // Rook H8 attacks horizontally
        assert!(attacked_by_white.contains(Square::G8)); // Rook H8 attacks horizontally
    }

    #[test]
    fn test_mask_castling_squares_attacked() {
        // Black to move. White rook A1 attacks C8, D8. Black rook H8 attacks F1, G1.
        let mut board = Board::from_fen("r3k2r/2Q5/8/8/8/8/8/R3K2R b KQkq - 0 1").unwrap();
        board.update_masks(); // Update masks for Black's turn (calculates attacks by White)

        let attacked_by_white = board.attacked();

        // White Rook A1 attacks Black's queenside castling path
        assert!(attacked_by_white.contains(Square::B8), "B8 attacked by Ra1");
        assert!(attacked_by_white.contains(Square::C8), "C8 attacked by Ra1");
        assert!(attacked_by_white.contains(Square::D8), "D8 attacked by Ra1");
        // E8 is king
        assert!(
            !attacked_by_white.contains(Square::F8),
            "F8 not attacked by Ra1"
        );
        assert!(
            !attacked_by_white.contains(Square::G8),
            "G8 not attacked by Ra1"
        );

        // Now check White's perspective (attacks by Black)
        let mut board_white_turn = Board::from_fen("4krr1/8/8/8/8/8/8/R3K2R w KQ - 0 1").unwrap();
        board_white_turn.update_masks(); // Update masks for White's turn (calculates attacks by Black)
        let attacked_by_black = board_white_turn.state.attacked;

        // Black Rook H8 attacks White's kingside castling path
        assert!(attacked_by_black.contains(Square::F1), "F1 attacked by Rh8");
        assert!(attacked_by_black.contains(Square::G1), "G1 attacked by Rh8");
        // E1 is king
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
        // White King E1, White Rook E2, Black Rook E8 (pins E2), Black Bishop A6.
        // White to move. Bishop A6 *would* check if Rook E2 moved, but E2 is pinned.
        let mut board = Board::from_fen("4rk2/8/b7/8/8/8/4R3/4K3 w - - 0 1").unwrap();
        board.update_masks();

        let expected_hv_pin = pin_bb(Square::E1, Square::E8); // E2-E7

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