use super::Board;
use super::attacks;
use super::movegen::aligned;
use crate::core::*;

impl Board {
    /// Adds a piece to the board at the specified square.
    ///
    /// Updates the board array (`self.board`), piece type bitboard (`self.pieces`),
    /// and colour occupied bitboard (`self.occupied`).
    ///
    /// **Note:** This method does *not* update the Zobrist key or any other board state
    /// like counters or castling rights. It's intended as a low-level helper.
    ///
    /// # Arguments
    ///
    /// * `piece` - The `Piece` to add.
    /// * `square` - The `Square` to add the piece to.
    #[inline]
    pub(crate) fn add_piece(&mut self, piece: Piece, square: Square) {
        // Put piece in the board
        self.board[square.index()] = Some(piece);
        // Update piece bitboards
        self.pieces[piece.pt().index()].set(square);
        self.occupied[piece.colour().index()].set(square);
    }

    /// Removes a piece from the board at the specified square.
    ///
    /// Updates the board array (`self.board`), piece type bitboard (`self.pieces`),
    /// and colour occupied bitboard (`self.occupied`). Assumes a piece exists at the square.
    ///
    /// **Note:** This method does *not* update the Zobrist key or any other board state
    /// like counters or castling rights. It's intended as a low-level helper.
    ///
    /// # Arguments
    ///
    /// * `square` - The `Square` to remove the piece from.
    ///
    /// # Panics
    ///
    /// Panics with `.expect` if `self.board[square]` is `None`.
    #[inline]
    pub(crate) fn remove_piece(&mut self, square: Square) {
        // Get the piece to remove
        debug_assert!(self.on(square).is_some(), "remove_piece: 'square' is empty");
        let piece = unsafe { self.on(square).unwrap_unchecked() };
        // Remove piece from the board
        self.board[square.index()] = None;
        // Update piece bitboards
        self.pieces[piece.pt().index()].clear(square);
        self.occupied[piece.colour().index()].clear(square);
    }

    /// Moves a piece from one square to another.
    ///
    /// Updates the board array (`self.board`), the piece type bitboard (`self.pieces`),
    /// and the colour occupied bitboard (`self.occupied`) using XOR operations for efficiency.
    /// Assumes a piece exists at the `from` square.
    ///
    /// **Note:** This method does *not* update the Zobrist key or any other board state
    /// like counters or castling rights. It's intended as a low-level helper.
    ///
    /// # Arguments
    ///
    /// * `from` - The `Square` to move the piece from.
    /// * `to` - The `Square` to move the piece to.
    ///
    /// # Panics
    ///
    /// Panics with `.expect` if `self.board[from]` is `None`.
    #[inline]
    pub(crate) fn move_piece(&mut self, from: Square, to: Square) {
        // Get the piece to move
        debug_assert!(
            self.on(from).is_some(),
            "move_piece: 'from' square is empty"
        );
        let piece = unsafe { self.on(from).unwrap_unchecked() };
        // Remove piece from the board
        self.board[from.index()] = None;
        // Put piece in the board
        self.board[to.index()] = Some(piece);
        // Update piece bitboards
        self.pieces[piece.pt().index()].clear(from);
        self.pieces[piece.pt().index()].set(to);
        // Update occupied bitboards
        self.occupied[piece.colour().index()].clear(from);
        self.occupied[piece.colour().index()].set(to);
    }

    /// Sets the en passant square based on a double pawn push originating `from`.
    ///
    /// Updates `self.state.enpassant` and toggles the Zobrist key (`self.state.key`)
    /// for the *new* en passant file. Assumes the move from `from` is a valid double push.
    ///
    /// # Arguments
    ///
    /// * `from` - The starting `Square` of the double pawn push.
    ///
    /// # Panics
    ///
    /// Panics with `.expect` if the calculated en passant square is invalid or `None`.
    #[inline]
    fn set_ep(&mut self, from: Square) {
        let us = self.stm;
        // Set Enpassant Square
        self.state.enpassant = Some(unsafe { from.add_unchecked(us.forward()) });
        // Toggle enpassant key (There must be an enpassant square after a double push)
        debug_assert!(
            self.state.enpassant.is_some(),
            "En passant square should exist after double push"
        );
        self.state
            .keys
            .toggle_ep(unsafe { self.state.enpassant.unwrap_unchecked().file() });
    }

    /// Calculates the starting square of the rook involved in castling.
    ///
    /// # Arguments
    ///
    /// * `king_side` - `true` for kingside castling (O-O), `false` for queenside (O-O-O).
    ///
    /// # Returns
    ///
    /// The `Square` where the rook starts (H1/H8 or A1/A8).
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

    /// Calculates the destination square of the rook involved in castling.
    ///
    /// # Arguments
    ///
    /// * `king_side` - `true` for kingside castling (O-O), `false` for queenside (O-O-O).
    ///
    /// # Returns
    ///
    /// The `Square` where the rook ends up (F1/F8 or D1/D8).
    #[inline]
    fn rook_to(&self, king_side: bool) -> Square {
        let us = self.stm;
        match king_side {
            true => Square::F1.relative(us),
            false => Square::D1.relative(us),
        }
    }

    /// Performs the rook movement part of castling.
    ///
    /// Moves the appropriate rook using `move_piece` and updates the Zobrist key (`self.state.key`)
    /// by toggling the rook piece at its `from` and `to` squares.
    ///
    /// **Note:** The king's movement and castling rights update are handled separately in `make_move`.
    ///
    /// # Arguments
    ///
    /// * `king_side` - `true` for kingside castling (O-O), `false` for queenside (O-O-O).
    #[inline]
    fn castle(&mut self, king_side: bool) {
        let us = self.stm;
        let piece = Piece::from_parts(us, PieceType::Rook);

        // Get the source and destination squares of the rook, given the side to move
        let rook_from = self.rook_from(king_side);
        let rook_to = self.rook_to(king_side);

        // Move the rook
        self.move_piece(rook_from, rook_to);

        // Update hash key for the rook's movement
        self.state.keys.toggle_piece(piece, rook_from);
        self.state.keys.toggle_piece(piece, rook_to);

        // Disclaimer: The King will be moved as part of the main make move function
    }

    /// Reverses the rook movement part of castling during `undo_move`.
    ///
    /// Moves the appropriate rook back to its original square using `move_piece`.
    ///
    /// **Note:** Zobrist key updates are *not* performed here, as the entire key is
    /// restored by `restore_state` in `undo_move`. The king's movement is also
    /// handled separately in `undo_move`.
    ///
    /// # Arguments
    ///
    /// * `king_side` - `true` for kingside castling (O-O), `false` for queenside (O-O-O).
    #[inline]
    fn undo_castle(&mut self, king_side: bool) {
        // Get the source and destination squares of the rook, given the side to move
        let rook_from = self.rook_from(king_side);
        let rook_to = self.rook_to(king_side);

        // Move the rook back
        self.move_piece(rook_to, rook_from);

        // Disclaimer: The King will be moved as part of the main make move function
    }

    /// Updates the castling rights based on a piece moving from `from` or to `to`.
    ///
    /// This is typically called when a king or rook moves, or when a rook is captured.
    /// It masks the current castling rights (`self.state.castle`) with the rights
    /// associated with the `from` and `to` squares. It also updates the Zobrist key
    /// (`self.state.key`) by toggling out the old rights and toggling in the new rights.
    ///
    /// # Arguments
    ///
    /// * `from` - The starting `Square` of the move.
    /// * `to` - The ending `Square` of the move.
    #[inline]
    fn update_castle_rights(&mut self, from: Square, to: Square) {
        // Remove previous castling rights from hash key
        self.state.keys.toggle_castle(self.state.castle);
        // Update castling rights by masking away rights affected by the move
        self.state
            .castle
            .mask(self.castling_rights(from) & self.castling_rights(to));
        // Add new castling rights to hash key
        self.state.keys.toggle_castle(self.state.castle);
    }

    /// Update Repetitions
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

    /// Applies a `Move` to the board, updating the state.
    ///
    /// This is the primary function for changing the board position. It handles:
    /// - Storing the current state for `undo_move`.
    /// - Updating piece positions and bitboards.
    /// - Handling captures (storing the captured piece).
    /// - Handling en passant captures.
    /// - Handling castling (moving king and rook).
    /// - Handling promotions.
    /// - Updating castling rights if a king or rook moves or is captured.
    /// - Clearing the previous en passant square and setting a new one on double pawn pushes.
    /// - Updating the fifty-move counter (resetting on pawn moves/captures, incrementing otherwise).
    /// - Incrementing the half-move counter.
    /// - Incrementally updating the Zobrist key for all changes (pieces, EP, castling, side-to-move).
    /// - Toggling the side to move.
    ///
    /// # Arguments
    ///
    /// * `move_` - The `Move` to apply. Assumed to be pseudo-legal or legal for the current position.
    pub fn make_move(&mut self, move_: Move) {
        // Cache the current state (becomes the previous state after this function)
        let state = self.state.snapshot();
        let old = std::mem::replace(&mut self.state, state);
        self.history.push(old);
        // Increment the half move counter (ply count)
        self.half_moves += 1;

        // Initialise variables
        let from = move_.from();
        let to = move_.to();
        let us = self.stm;
        let them = !us;
        // debug_assert!(self.on(from).is_some(), "make_move: 'from' square is empty");
        let piece = unsafe { self.on(from).unwrap_unchecked() }; // Piece being moved
        let flag = move_.flag();

        // Increment fifty-move counter by default. It will be reset below if applicable.
        self.state.fifty_move += 1;

        // Clear previous en passant square from state and Zobrist key if it exists.
        // Must be done *before* potentially setting a new one.
        if let Some(ep_sq) = self.state.enpassant {
            self.state.keys.toggle_ep(ep_sq.file());
            self.state.enpassant = None;
        }

        // Handle the specific move type based on its flag
        match flag {
            // Handle Quiet Moves
            MoveFlag::QuietMove => {
                if piece.pt() == PieceType::Pawn {
                    // Reset fifty move counter for pawn moves
                    self.state.fifty_move = 0;
                }
                // Move the piece on board and bitboards
                self.move_piece(from, to);
                // Update hash keys for piece movement
                self.state.keys.toggle_piece(piece, from);
                self.state.keys.toggle_piece(piece, to);
                // Update castling rights if king/rook moved from/to relevant squares
                self.update_castle_rights(from, to);
            }
            // Handle Double Pawn Pushes
            MoveFlag::DoublePawnPush => {
                // Reset fifty move counter for pawn moves
                self.state.fifty_move = 0;
                // Set the new en passant square and update its Zobrist key
                self.set_ep(from);
                // Move the pawn on board and bitboards
                self.move_piece(from, to);
                // Update hash keys for pawn movement
                self.state.keys.toggle_piece(piece, from);
                self.state.keys.toggle_piece(piece, to);
                // Note: Castling rights are not affected by pawn moves.
            }
            // Handle Castling
            MoveFlag::KingCastle | MoveFlag::QueenCastle => {
                // Reset fifty move counter (castling is a king move, not pawn/capture)
                // self.state.fifty_move += 1; // Already incremented above
                // Remove the king (Pick it up, prevent bug where the destination of the rook is on the square of the king)
                self.remove_piece(from);
                // Update hash keys for king movement
                self.state.keys.toggle_piece(piece, from);
                let is_king_side = flag == MoveFlag::KingCastle;
                // Move the rook and update its Zobrist keys
                self.castle(is_king_side);
                // Add the king (Place it back down)
                self.add_piece(piece, to);
                // Update hash keys for king movement
                self.state.keys.toggle_piece(piece, to);
                // Update castling rights (king move always removes rights for that side)
                // The `from` square (e1/e8) is sufficient to remove rights.
                self.update_castle_rights(from, to); // `to` doesn't affect rights here, but harmless
            }
            // Handle Capture
            MoveFlag::Capture => {
                // Reset fifty move counter for captures
                self.state.fifty_move = 0;
                // Store captured piece (must exist at 'to' square)
                debug_assert!(
                    self.on(to).is_some(),
                    "make_move: Capture flag set, but 'to' square is empty"
                );
                let captured_piece = unsafe { self.on(to).unwrap_unchecked() };
                self.state.captured = Some(captured_piece);
                // Remove the captured piece from board/bitboards
                self.remove_piece(to);
                // Update hash keys for the removed captured piece
                self.state.keys.toggle_piece(captured_piece, to);
                // Move the attacking piece
                self.move_piece(from, to);
                // Update hash keys for the moved attacking piece
                self.state.keys.toggle_piece(piece, from);
                self.state.keys.toggle_piece(piece, to);
                // Update castling rights if king/rook moved or if a rook was captured on its home square
                self.update_castle_rights(from, to);
            }
            // Handle Enpassant
            MoveFlag::EPCapture => {
                // Reset fifty move counter for captures (en passant is a pawn capture)
                self.state.fifty_move = 0;
                // Calculate the square of the pawn being captured enpassant (Safe as en_passant squares can only be on Rank 2 or 6)
                debug_assert!(
                    to.add(-us.forward()).is_ok(),
                    "make_move: Invalid EP target square calculation"
                );
                let cap_sq = unsafe { to.add(-us.forward()).unwrap_unchecked() };
                let captured_pawn = Piece::from_parts(them, PieceType::Pawn);
                // Store the captured pawn type (always a pawn of the opposite color)
                self.state.captured = Some(captured_pawn);
                // Remove the captured pawn from board/bitboards
                self.remove_piece(cap_sq);
                // Update hash keys for the removed captured pawn
                self.state.keys.toggle_piece(captured_pawn, cap_sq);
                // Move the attacking pawn
                self.move_piece(from, to);
                // Update hash keys for the moved attacking pawn
                self.state.keys.toggle_piece(piece, from);
                self.state.keys.toggle_piece(piece, to);
                // Note: Castling rights are not affected by EP captures.
            }

            // Promotion without capture
            MoveFlag::KnightPromo
            | MoveFlag::BishopPromo
            | MoveFlag::RookPromo
            | MoveFlag::QueenPromo => {
                // Reset fifty move counter for pawn moves
                self.state.fifty_move = 0;
                // Get the piece type to promote to from the move flag
                let promo_pt = move_.promotion_pt(); // Assumes MoveFlag maps correctly
                let promo_piece = Piece::from_parts(us, promo_pt);
                // Remove the pawn from its starting square
                self.remove_piece(from);
                // Update hash keys for the removed pawn
                self.state.keys.toggle_piece(piece, from); // `piece` is the pawn here
                // Add the promoted piece to the destination square
                self.add_piece(promo_piece, to);
                // Update hash keys for the added promoted piece
                self.state.keys.toggle_piece(promo_piece, to);
                // Update castling rights if promotion occurs on a rook's home square (rare, but possible)
                self.update_castle_rights(from, to);
            }

            // Promotion with capture
            MoveFlag::KnightPromoCapture
            | MoveFlag::BishopPromoCapture
            | MoveFlag::RookPromoCapture
            | MoveFlag::QueenPromoCapture => {
                // Reset fifty move counter for captures (and pawn moves)
                self.state.fifty_move = 0;
                // Store captured piece (must exist at 'to' square)
                debug_assert!(
                    self.on(to).is_some(),
                    "make_move: PromoCapture flag set, but 'to' square is empty"
                );
                let captured_piece = unsafe { self.on(to).unwrap_unchecked() };
                self.state.captured = Some(captured_piece);
                // Remove the captured piece
                self.remove_piece(to);
                // Update hash key for the removed captured piece
                self.state.keys.toggle_piece(captured_piece, to);
                // Get the piece type to promote to
                let promo_pt = move_.promotion_pt();
                let promo_piece = Piece::from_parts(us, promo_pt);
                // Remove the pawn from its starting square
                self.remove_piece(from);
                // Update hash key for the removed pawn
                self.state.keys.toggle_piece(piece, from); // `piece` is the pawn
                // Add the promoted piece to the destination square
                self.add_piece(promo_piece, to);
                // Update hash key for the added promoted piece
                self.state.keys.toggle_piece(promo_piece, to);
                // Update castling rights if promotion/capture occurs on a rook's home square
                self.update_castle_rights(from, to);
            }
        }

        // Toggle side to move *after* all other updates
        self.stm = !self.stm;
        // Update hash key for the change in side to move
        self.state.keys.toggle_colour();
        // Update masks
        self.update_masks();
        // Update repetitions
        self.update_repetitions();
    }

    /// Reverses a `Move` that was just made, restoring the previous board state.
    ///
    /// This function relies heavily on the state stored by the preceding `make_move` call.
    /// It performs the following actions:
    /// - Toggles the side to move back.
    /// - Decrements the half-move counter.
    /// - Restores the *entire* previous state (`fifty_move`, `captured`, `enpassant`, `castle`, Zobrist `key`)
    ///   from the history using `restore_state`. This implicitly handles Zobrist key restoration.
    /// - Reverses the piece movements based on the `MoveFlag`:
    ///   - Moves pieces back to their original squares.
    ///   - Adds captured pieces back to the board (using the `captured` piece from the restored state).
    ///   - Reverses promotions (removes promoted piece, adds pawn back).
    ///   - Reverses castling (moves king and rook back).
    ///
    /// # Arguments
    ///
    /// * `move_` - The *exact* `Move` object that was previously applied using `make_move`.
    ///
    /// # Important Assumptions
    ///
    /// - This function should only be called after a move has been made (or else there might be a stack underflow or mismatched moves)
    /// - The `move_` argument *must* be identical to the one passed to `make_move`.
    /// - The board state must not have been modified between the `make_move` and `undo_move` calls.
    pub fn undo_move(&mut self, move_: Move) {
        // Toggle side to move back to the state *before* the move was made
        self.stm = !self.stm;

        // Decrement the half move counter (ply count)
        self.half_moves -= 1;

        // Initialise variables needed *before* restoring state
        let from = move_.from();
        let to = move_.to();
        let us = self.stm; // Side who made the move being undone
        let flag = move_.flag();
        let captured = self.state.captured; // Get the captured piece *before* restoring state

        // Restore the entire previous state (key, counters, ep, castle, captured piece)
        // This must happen *before* moving pieces back, especially to retrieve `state.captured`.

        self.state = self.history.pop().unwrap();

        // Reverse the piece movements based on the move flag
        match flag {
            // Handle Quiet Moves and Double Pawn Pushes
            MoveFlag::QuietMove | MoveFlag::DoublePawnPush => {
                // Move the piece back from 'to' to 'from'
                self.move_piece(to, from);
                // En passant square (if any existed before this move) is restored by restore_state
                // Castling rights are restored by restore_state
            }
            // Handle Captures
            MoveFlag::Capture => {
                // Move the attacking piece back
                self.move_piece(to, from);
                // Add the captured piece (retrieved from restored state) back to the 'to' square
                debug_assert!(
                    captured.is_some(),
                    "undo_move: Capture flag set, but restored state has no captured piece"
                );
                self.add_piece(unsafe { captured.unwrap_unchecked() }, to);
                // Castling rights restored by restore_state
            }
            // Handle Enpassant
            MoveFlag::EPCapture => {
                // Move the attacking pawn back
                self.move_piece(to, from);
                // Calculate the square where the EP captured pawn was
                debug_assert!(
                    to.add(-us.forward()).is_ok(),
                    "undo_move: Invalid EP target square calculation"
                );
                let cap_sq = unsafe { to.add(-us.forward()).unwrap_unchecked() };
                // Add the captured pawn (always Pawn of opposite color) back
                debug_assert!(
                    captured.is_some(),
                    "undo_move: EPCapture flag set, but restored state has no captured piece"
                );
                self.add_piece(unsafe { captured.unwrap_unchecked() }, cap_sq);
                // En passant square restored by restore_state
            }
            // Handle Castling
            MoveFlag::KingCastle | MoveFlag::QueenCastle => {
                let is_king_side = flag == MoveFlag::KingCastle;
                // Pick up the king
                self.remove_piece(to);
                // Move the rook back (undo_castle uses move_piece internally)
                self.undo_castle(is_king_side);
                // Place the king down
                self.add_piece(Piece::from_parts(us, PieceType::King), from);
                // Castling rights are restored by restore_state
            }
            // Promotion without capture
            MoveFlag::KnightPromo
            | MoveFlag::BishopPromo
            | MoveFlag::RookPromo
            | MoveFlag::QueenPromo => {
                // Remove the promoted piece from the 'to' square
                self.remove_piece(to);
                // Add the original pawn back to the 'from' square
                self.add_piece(Piece::from_parts(us, PieceType::Pawn), from);
                // Castling rights restored by restore_state
            }
            // Promotion with capture
            MoveFlag::KnightPromoCapture
            | MoveFlag::BishopPromoCapture
            | MoveFlag::RookPromoCapture
            | MoveFlag::QueenPromoCapture => {
                // Remove the promoted piece from the 'to' square
                self.remove_piece(to);
                // Add the captured piece (from restored state) back to the 'to' square
                debug_assert!(
                    captured.is_some(),
                    "undo_move: PromoCapture flag set, but restored state has no captured piece"
                );
                self.add_piece(unsafe { captured.unwrap_unchecked() }, to);
                // Add the original pawn back to the 'from' square
                self.add_piece(Piece::from_parts(us, PieceType::Pawn), from);
                // Castling rights restored by restore_state
            }
        }
        // Note: No Zobrist key toggling is needed here, as restore_state reset the key entirely.
        // Note: The .unwrap() in restore_state itself remains, as popping from an empty history
        // is considered a fatal logic error in the engine's operation (calling undo without a prior make).
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

        // Check if there is a piece to move and the from square is not the to square (except for chess960 castling moves), and there is no friendly fire
        if !self.on(from).is_some_and(|p| p.colour() == us) || (!move_.is_castle() && from == to) {
            return false;
        }

        if !move_.is_castle() && captured.is_some_and(|p| p.colour() == us) {
            return false;
        }

        let piece = self.on(from).unwrap();
        let piece_type = piece.pt();

        // Check if the flags are wrong, like capture moves not capturing, quiet moves capturing, or pawn moves not moving a pawn etc
        if (move_.is_capture() != captured.is_some()
            && !move_.is_ep_capture()
            && !move_.is_castle())
            || (is_pawn_move && piece_type != PieceType::Pawn)
        {
            return false;
        }

        // Check if the move is a promotion and the piece is on relative rank 7
        if move_.is_promotion() && from.relative(us).rank() != Rank::Rank7 {
            return false;
        }

        // If the move is a castling move, check each castling side to see if the rook can castle
        if move_.is_castle() {
            // if the to square is correct and the castling rights are set, then check if the king can actually castle
            if to.file() == File::FileG && self.castling().has(Castling::king_side(us)) {
                return !self.in_check() && self.can_castle(Castling::king_side(us));
            }

            // if the to square is correct and the castling rights are set, then check if the king can actually castle
            if to.file() == File::FileC && self.castling().has(Castling::queen_side(us)) {
                return !self.in_check() && self.can_castle(Castling::queen_side(us));
            }

            return false;
        }

        // Non pawn move must have the moving piece attacking the to square
        if piece_type != PieceType::Pawn {
            if !attacks(us, piece_type, from, all_occ).contains(to) {
                return false;
            }
        } else {
            // If move is an ep capture, handle it differently
            if move_.is_ep_capture() {
                if self.ep_target().is_none() || self.ep_pin() {
                    return false;
                }
                let ep_target = self.ep_target().unwrap();
                // If the to square is not the enpassant target or the pawn cannot legal move to that square, the move is illegal
                if to != ep_target
                    && (attacks(us, PieceType::Pawn, from, Bitboard::EMPTY) & to.bb()).is_empty()
                {
                    return false;
                }
                // Quiet pawn moves, so pushes and double pushes
            } else if !move_.is_capture() {
                // Double pushes should only start on relative rank 2
                if move_.is_double_push() && from.relative(us).rank() != Rank::Rank2 {
                    return false;
                }
                // Quiet pawn moves
                let forward = us.forward();
                let push_sq = unsafe { from.add_unchecked(forward) };
                // The square in front should be clear
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

        // If the piece type is not a king, check if the destination blocks a check or captures the checker
        if piece_type != PieceType::King {
            if !self.check_mask().contains(to)
                && !(move_.is_ep_capture() && self.check_mask().contains(self.ep_target().unwrap()))
            {
                return false;
            }
        // If the piece type is a king, check if the king is stepping into danger
        } else {
            return !self.attacked().contains(to);
        }

        let diag_pin = self.diag_pin();
        let hv_pin = self.hv_pin();

        // --- Flag the non king piece for one of 3 states ---
        let ksq = self.ksq(us);
        // If it is diagonally pinned, then the from and to square must be on the pin mask
        let diag_pinned =
            diag_pin.contains(from) && diag_pin.contains(to) && aligned(from, to, ksq);
        // If it is vertically pinned, then the from and to square must be on the pin mask
        let hv_pinned = hv_pin.contains(from) && hv_pin.contains(to) && aligned(from, to, ksq);
        // If its not pinned, then it can move wherever it wants
        let not_pinned = !(diag_pin.contains(from) || hv_pin.contains(from));

        return diag_pinned || hv_pinned || not_pinned;
    }

    pub fn is_capture(&self, move_: Move) -> bool {
        move_.is_valid() && self.on(move_.to()).is_some()
    }
}

#[cfg(test)]
mod tests {
    // ... tests remain the same ...
    // Note: The .unwrap() calls within the test setup functions (like board_from_fen)
    // are generally acceptable, as a panic there indicates a problem with the test itself.
    // The .expect() calls added above are primarily for the core board logic.
    use super::*; // Import Board and its methods
    use crate::board::fen::*; // Import FEN constants and Board::from_fen

    // Helper function to create a board and calculate its keys consistently
    fn board_from_fen(fen: &str) -> Board {
        let board = Board::from_fen(fen).expect("Test FEN should be valid");
        // Ensure the keys stored after parsing match a fresh calculation
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

    // Helper function to perform make/undo and check board state + keys consistency
    fn test_make_undo(fen_before: &str, move_to_test: Move, fen_after: &str) {
        let mut board = board_from_fen(fen_before);
        let key_before = board.state.keys.key;
        let pawn_key_before = board.state.keys.pawn_key;
        let non_pawn_key_before = board.state.keys.non_pawn_key;

        // --- Make Move ---
        board.make_move(move_to_test);
        let key_after_make = board.state.keys.key;
        let pawn_key_after_make = board.state.keys.pawn_key;
        let non_pawn_key_after_make = board.state.keys.non_pawn_key;

        // Assert board state after make_move
        assert_eq!(
            board.fen(),
            fen_after,
            "FEN mismatch after make_move for '{}'",
            move_to_test
        );
        // Assert main key is different
        assert_ne!(
            key_before, key_after_make,
            "Key should change after make_move for '{}'",
            move_to_test
        );

        // --- Verify Pawn Key Change (or lack thereof) ---
        let expected_pawn_key_after = Board::from_fen(fen_after).unwrap().calc_pawn_key();
        assert_eq!(
            pawn_key_after_make, expected_pawn_key_after,
            "Pawn key incorrect after make_move for '{}'. Expected {:?}, Got {:?}",
            move_to_test, expected_pawn_key_after, pawn_key_after_make
        );
        let expected_non_pawn_key_after = Board::from_fen(fen_after).unwrap().calc_non_pawn_key();
        assert_eq!(
            non_pawn_key_after_make, expected_non_pawn_key_after,
            "Pawn key incorrect after make_move for '{}'. Expected {:?}, Got {:?}",
            move_to_test, expected_non_pawn_key_after, non_pawn_key_after_make
        );
        // Optionally assert if pawn key *should* have changed
        let piece = Board::from_fen(fen_before)
            .unwrap()
            .on(move_to_test.from())
            .unwrap();
        let captured_piece = if move_to_test.flag().is_capture() {
            Board::from_fen(fen_before).unwrap().on(move_to_test.to())
        } else if move_to_test.is_ep_capture() {
            Some(Piece::from_parts(!board.stm(), PieceType::Pawn)) // After make_move, side is flipped
        } else {
            None
        };

        let affects_pawn_key = piece.pt() == PieceType::Pawn
            || captured_piece.map_or_else(|| false, |p| p.pt() == PieceType::Pawn);
        if affects_pawn_key {
            assert_ne!(
                pawn_key_before, pawn_key_after_make,
                "Pawn key should change after pawn-related move '{}'",
                move_to_test
            );
        } else {
            assert_eq!(
                pawn_key_before, pawn_key_after_make,
                "Pawn key should NOT change after non-pawn move '{}'",
                move_to_test
            );
        }

        // --- Undo Move ---
        board.undo_move(move_to_test);
        let key_after_undo = board.state.keys.key;
        let pawn_key_after_undo = board.state.keys.pawn_key;
        let non_pawn_key_after_undo = board.state.keys.non_pawn_key;

        // Assert board state is restored
        assert_eq!(
            board.fen(),
            fen_before,
            "FEN mismatch after undo_move for '{}'",
            move_to_test
        );
        // Assert keys are restored exactly
        assert_eq!(
            key_after_undo, key_before,
            "Key mismatch after undo_move for '{}'",
            move_to_test
        );
        assert_eq!(
            pawn_key_after_undo, pawn_key_before,
            "Pawn key mismatch after undo_move for '{}'",
            move_to_test
        );
        assert_eq!(
            non_pawn_key_after_undo, non_pawn_key_before,
            "Non Pawn key mismatch after undo_move for '{}'",
            move_to_test
        );

        // --- Optional: Verify key calculation consistency after undo ---
        let calculated_key_after_undo = board.calc_key();
        let calculated_pawn_key_after_undo = board.calc_pawn_key();
        assert_eq!(
            calculated_key_after_undo, key_before,
            "Recalculated key mismatch after undo_move for '{}'",
            move_to_test
        );
        assert_eq!(
            calculated_pawn_key_after_undo, pawn_key_before,
            "Recalculated pawn key mismatch after undo_move for '{}'",
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
            "N1bqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQk - 0 6", // White Knight on a8
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
        assert_eq!(board.state.fifty_move, 0); // Initial state

        // Quiet pawn move
        board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
        assert_eq!(board.state.fifty_move, 0); // Reset on pawn move
        board.undo_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));

        // Capture
        board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::D7, Square::D5, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::E4, Square::D5, MoveFlag::Capture));
        assert_eq!(board.state.fifty_move, 0); // Reset on capture
        board.undo_move(Move::new(Square::E4, Square::D5, MoveFlag::Capture));
        board.undo_move(Move::new(Square::D7, Square::D5, MoveFlag::DoublePawnPush));
        board.undo_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));

        // Quiet knight move
        board.make_move(Move::new(Square::G1, Square::F3, MoveFlag::QuietMove));
        assert_eq!(board.state.fifty_move, 1); // Increment on non-pawn, non-capture
        board.undo_move(Move::new(Square::G1, Square::F3, MoveFlag::QuietMove));

        // Quiet king move
        board.make_move(Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::E7, Square::E5, MoveFlag::DoublePawnPush));
        board.make_move(Move::new(Square::E1, Square::E2, MoveFlag::QuietMove));
        assert_eq!(board.state.fifty_move, 1); // Increment on non-pawn, non-capture
    }

    #[test]
    fn test_quiet_pawn_move_keys() {
        test_make_undo(
            START_FEN,
            Move::new(Square::E2, Square::E4, MoveFlag::DoublePawnPush), // e2e4
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
        );
    }

    #[test]
    fn test_quiet_knight_move_keys() {
        test_make_undo(
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1", // After 1. e4
            Move::new(Square::G8, Square::F6, MoveFlag::QuietMove),        // ...Nf6
            "rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2",
        );
    }

    #[test]
    fn test_capture_pawn_takes_pawn_keys() {
        test_make_undo(
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2", // After 1.e4 d5
            Move::new(Square::E4, Square::D5, MoveFlag::Capture),            // exd5
            "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2",
        );
    }

    #[test]
    fn test_capture_knight_takes_pawn_keys() {
        test_make_undo(
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2", // After 1.e4 d5 2.Nf3
            Move::new(Square::F3, Square::D4, MoveFlag::QuietMove), // Need a setup where knight takes pawn
            "rnbqkbnr/ppp1pppp/8/3p4/3NP3/8/PPPP1PPP/RNBQKB1R w KQkq - 2 3", // Example setup
        );
        // Actual test
        test_make_undo(
            "r1bqkbnr/ppp1pppp/2n5/3p4/3NP3/8/PPPP1PPP/RNBQKB1R b KQkq - 3 3", // After 1.e4 d5 2.Nf3 Nc6 3.Nd4
            Move::new(Square::C6, Square::D4, MoveFlag::Capture),              // Nxd5
            "r1bqkbnr/ppp1pppp/8/3p4/3nP3/8/PPPP1PPP/RNBQKB1R w KQkq - 0 4",   // Corrected FEN
        );
    }

    #[test]
    fn test_en_passant_capture_keys() {
        let fen_before_ep = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3";
        test_make_undo(
            fen_before_ep,
            Move::new(Square::E5, Square::D6, MoveFlag::EPCapture), // White captures EP
            "rnbqkbnr/ppp1pppp/3P4/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3",
        );
    }

    #[test]
    fn test_white_kingside_castle_keys() {
        let fen_before_castle = "rnbq1bnr/pppppkpp/8/8/8/8/PPPPPPPP/RNBQK2R w KQ - 0 5";
        test_make_undo(
            fen_before_castle,
            Move::new(Square::E1, Square::G1, MoveFlag::KingCastle), // O-O
            "rnbq1bnr/pppppkpp/8/8/8/8/PPPPPPPP/RNBQ1RK1 b - - 1 5",
        );
    }

    #[test]
    fn test_promotion_quiet_keys() {
        let fen_before_promo = "r1bqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6";
        test_make_undo(
            fen_before_promo,
            Move::new_promotion(Square::B7, Square::B8, PieceType::Queen, false), // b7b8=Q
            "rQbqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 6",
        );
    }

    #[test]
    fn test_promotion_capture_pawn_keys() {
        // Pawn takes pawn promotion
        let fen_before_promo_cap = "rnbqkbnr/ppPppppp/8/8/8/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1"; // White C7, Black D7
        test_make_undo(
            fen_before_promo_cap,
            Move::new_promotion(Square::C7, Square::D8, PieceType::Queen, true), // cxd8=Q
            "rnbQkbnr/pp1ppppp/8/8/8/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1",
        );
    }

    #[test]
    fn test_promotion_capture_non_pawn_keys() {
        // Pawn takes rook promotion
        let fen_before_promo_cap = "rnbqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6"; // White b7, Black a8
        test_make_undo(
            fen_before_promo_cap,
            Move::new_promotion(Square::B7, Square::A8, PieceType::Knight, true), // bxa8=N
            "Nnbqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQk - 0 6",
        );
    }

    // --- Chess960 Castling Tests ---

    #[test]
    fn test_xfen_white_kingside_castle_k_e1_r_g1() {
        // Setup: King E1, Rook G1. White has 'G' castling right (maps to WK).
        let fen_before = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/4KR2 w K - 0 1";
        // Move: White castles kingside (O-O). King E1->G1, Rook G1->F1.
        let castle_move = Move::new(Square::E1, Square::G1, MoveFlag::KingCastle);
        // Expected state: King G1, Rook F1. Side black. Castling rights '-'. Halfmove 1, Fullmove 1.
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
        // Setup: King E8, Rook F8. Black has 'f' castling right (maps to BK).
        let fen_before = "4kr2/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b k - 0 1";
        // Move: Black castles kingside (O-O). King E8->G8, Rook F8->F8 (standard dest).
        let castle_move = Move::new(Square::E8, Square::G8, MoveFlag::KingCastle);
        // Expected state: King G8, Rook F8. Side white. Castling rights '-'. Halfmove 1, Fullmove 2.
        let fen_after = "5rk1/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 1 2";
        test_make_undo(fen_before, castle_move, fen_after);
    }

    #[test]
    fn test_xfen_black_queenside_castle_k_e8_r_c8() {
        // Setup: King E8, Rook C8. Black has 'c' castling right (maps to BQ).
        let fen_before = "2r1k3/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b q - 0 1";
        // Move: Black castles queenside (O-O-O). King E8->C8, Rook C8->D8.
        let castle_move = Move::new(Square::E8, Square::C8, MoveFlag::QueenCastle);
        // Expected state: King C8, Rook D8. Side white. Castling rights '-'. Halfmove 1, Fullmove 2.
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
        let castle_move = Move::new(Square::F1, Square::C1, MoveFlag::QueenCastle); // King G1 -> C1
        let fen_after = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/2KR2R1 b - - 1 1";
        test_make_undo(fen_before, castle_move, fen_after);
    }

    // --- Chess960 Castling Rights Removal Tests ---

    #[test]
    fn test_xfen_rights_removal_king_move() {
        let fen_before = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQ2KR w KQ - 0 1";
        let king_move = Move::new(Square::G1, Square::F1, MoveFlag::QuietMove);
        let fen_after = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQ1K1R b - - 1 1";
        test_make_undo(fen_before, king_move, fen_after);
    }

    #[test]
    fn test_xfen_rights_removal_h_rook_move() {
        // Setup: SP 4 (RNBQKB1R). King G1, Rooks F1, H1. White has 'F' (WQ) and 'H' (WK) rights.
        let fen_before = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKB1R w KQ - 0 1";
        // Move: H-Rook (WK side) moves H1->H2.
        let rook_move = Move::new(Square::H1, Square::G1, MoveFlag::QuietMove);
        let fen_after = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBR1 b Q - 1 1"; // Note: Castling right is now just 'F'
        test_make_undo(fen_before, rook_move, fen_after);
    }

    // --- XFen Move Tests ---
    #[test]
    fn test_xfen_custom_position() {
        let fen_before = "rn2k1r1/ppp1pp1p/3p2p1/5bn1/P7/2N2B2/1PPPPP2/2BNK1RR w Gkq - 4 11";
        let fen_after = "rn2k1r1/ppp1pp1p/3p2p1/5bn1/P6R/2N2B2/1PPPPP2/2BNK1R1 b Kkq - 5 11";
        let rook_move = Move::new(Square::H1, Square::H4, MoveFlag::QuietMove);
        test_make_undo(fen_before, rook_move, fen_after);
    }

    // Helper to check repetition count
    fn assert_repetitions(board: &Board, expected: i8) {
        // NOTE: This assertion relies on the update_repetitions logic correctly
        // finding repetitions in the history by comparing Zobrist keys.
        // It also depends on the history scanning depth logic (interaction with fifty_move).
        // If that logic is flawed, this test might pass incorrectly or fail unexpectedly.
        assert_eq!(board.state.repetitions, expected);
    }

    #[test]
    fn test_three_fold_repetition() {
        // Use the existing helper that ensures keys are calculated correctly initially
        let mut board = board_from_fen(START_FEN);

        // Sequence = 1. Nf3 Nc6 2. Ng1 Nb8 3. Nf3 Nc6 4. Ng1 Nb8 5. Nf3 Nc6 6. Nd4 Nb8 7. Nf3 Nc6

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
