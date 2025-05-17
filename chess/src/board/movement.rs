use std::usize;

use super::Board;
use super::attacks;
use super::movegen::aligned;
use crate::core::*;

impl Board {
    /******************************************\
    |==========================================|
    |         Basic Piece Manipulations        |
    |==========================================|
    \******************************************/

    /// Adds a piece to the board at the given square.
    ///
    /// This updates the main board array, the piece-specific bitboards,
    /// and the color-specific occupancy bitboards.
    #[inline]
    pub(crate) fn add_piece(&mut self, piece: Piece, square: Square) {
        self.board[square.index()] = Some(piece);
        self.pieces[piece.pt().index()].set(square);
        self.occupied[piece.colour().index()].set(square);
    }

    /// Removes a piece from the board at the given square.
    ///
    /// This updates the main board array, the piece-specific bitboards,
    /// and the color-specific occupancy bitboards.
    /// Panics in debug if the square is empty.
    #[inline]
    pub(crate) fn remove_piece(&mut self, square: Square) {
        debug_assert!(self.on(square).is_some(), "remove_piece: 'square' is empty");
        let piece = unsafe { self.on(square).unwrap_unchecked() };

        self.board[square.index()] = None;

        self.pieces[piece.pt().index()].clear(square);
        self.occupied[piece.colour().index()].clear(square);
    }

    /// Moves a piece from a 'from' square to a 'to' square.
    ///
    /// This updates the main board array, the piece-specific bitboards,
    /// and the color-specific occupancy bitboards for the moved piece.
    /// Panics in debug if the 'from' square is empty.
    #[inline]
    pub(crate) fn move_piece(&mut self, from: Square, to: Square) {
        let piece = unsafe { self.on(from).unwrap_unchecked() };

        // update board array
        self.board[from.index()] = None;
        self.board[to.index()] = Some(piece);

        // update piece bitboards
        self.pieces[piece.pt().index()].clear(from);
        self.pieces[piece.pt().index()].set(to);

        // update occupied bitboards
        self.occupied[piece.colour().index()].clear(from);
        self.occupied[piece.colour().index()].set(to);
    }

    /******************************************\
    |==========================================|
    |           Board Movement Helpers         |
    |==========================================|
    \******************************************/

    /// Set enpassant square and updates the hash key
    #[inline]
    fn set_ep(&mut self, from: Square) {
        let us = self.stm;

        self.state.enpassant = Some(unsafe { from.add_unchecked(us.forward()) });
        self.state
            .keys
            .toggle_ep(unsafe { self.state.enpassant.unwrap_unchecked().file() });
    }

    /// Get the rook's source square
    #[inline]
    fn rook_from(&self, king_side: bool) -> Square {
        let us = self.stm;
        let index = us.index() * 2 + !king_side as usize;
        unsafe { self.castling_mask.rook_sq[index].unwrap_unchecked() }
    }

    /// Get the rook's destination square
    #[inline]
    fn rook_to(&self, king_side: bool) -> Square {
        let us = self.stm;
        match king_side {
            true => Square::F1.relative(us),
            false => Square::D1.relative(us),
        }
    }

    /// Updates the castling rights by referencing the castling mask table and toggling the main key
    #[inline]
    fn update_castle_rights(&mut self, from: Square, to: Square) {
        self.state.keys.toggle_castle(self.state.castle);

        self.state
            .castle
            .mask(self.castling_rights(from) & self.castling_rights(to));

        self.state.keys.toggle_castle(self.state.castle);
    }

    /// Updates the repetition counter based on Zobrist keys in the game history.
    ///
    /// This function checks if the current board position has occurred previously.
    /// It iterates backwards through past positions where it was the current side's
    /// turn to move, within the window of the fifty-move rule.
    ///
    /// - If the current position matches a past position `S_past`:
    ///     - If no match is found, `self.state.repetitions` is set to `0`.
    ///     - If a match is found and `S_past.repetitions = 0` (Meaning S_Past has not been repeated), `self.state.repetitions` is set to `ply`.
    ///     - if a match is found and `S_past.repetitions > 0` (Meaning S_Past has been repeated once), `self.state.repetitions` is set to `-ply`.
    ///
    /// - This means that the is_draw function only has to check if `self.state.repetitions != 0` and `self.state.repetitions < ply`.
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

        // Iterate through at most 50 previous positions reached to see if there is a 3 fold repetitions (ignore those 50+ plies before because there must be an irreversible move in between)
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

    /******************************************\
    |==========================================|
    |             Main Move Helpers            |
    |==========================================|
    \******************************************/

    #[inline]
    fn do_quiet(&mut self, piece: Piece, from: Square, to: Square) {
        // Reset fifty move counter if the piece that moved is a pawn
        if piece.pt() == PieceType::Pawn {
            self.state.fifty_move = 0;
        }
        // Move the piece
        self.move_piece(from, to);
        // Update hash keys
        self.state.keys.toggle_piece(piece, from);
        self.state.keys.toggle_piece(piece, to);

        self.update_castle_rights(from, to);
    }

    #[inline]
    fn do_double_pawn_push(&mut self, piece: Piece, from: Square, to: Square) {
        // Reset the fifty move counter
        self.state.fifty_move = 0;
        // Update ep square and move the pawn
        self.set_ep(from);
        self.move_piece(from, to);
        // Update hash keys
        self.state.keys.toggle_piece(piece, from);
        self.state.keys.toggle_piece(piece, to);
    }

    #[inline]
    fn do_capture(&mut self, piece: Piece, from: Square, to: Square) {
        // Reset fifty move counter
        self.state.fifty_move = 0;
        // Update captured piece in board state
        let captured_piece = unsafe { self.on(to).unwrap_unchecked() };
        self.state.captured = Some(captured_piece);
        // Remove the captured piece and move the piece
        self.remove_piece(to);
        self.move_piece(from, to);
        // Update hash keys
        self.state.keys.toggle_piece(piece, from);
        self.state.keys.toggle_piece(piece, to);
        self.state.keys.toggle_piece(captured_piece, to);

        self.update_castle_rights(from, to);
    }

    #[inline]
    fn do_ep_capture(&mut self, piece: Piece, from: Square, to: Square) {
        let us = self.stm;
        let them = !us;
        // Reset fifty move counter
        self.state.fifty_move = 0;
        // Calculate the square of the captured pawn
        let ep_target = unsafe { to.add_unchecked(-us.forward()) };
        let captured_pawn = Piece::from_parts(them, PieceType::Pawn);
        // Update captured piece in board state
        self.state.captured = Some(captured_pawn);
        // Remove the captured piece and move the piece
        self.remove_piece(ep_target);
        self.move_piece(from, to);
        // Update hash keys
        self.state.keys.toggle_piece(captured_pawn, ep_target);
        self.state.keys.toggle_piece(piece, from);
        self.state.keys.toggle_piece(piece, to);
    }

    #[inline]
    fn do_quiet_promo(&mut self, piece: Piece, from: Square, to: Square, promo_pt: PieceType) {
        let us = self.stm;
        // Reset fifty move counter
        self.state.fifty_move = 0;
        // Calculate the promoted piece
        let promo_piece = Piece::from_parts(us, promo_pt);
        // Remove pawn and replace the to square with a promoted piece
        self.remove_piece(from);
        self.add_piece(promo_piece, to);
        // Update hash keys
        self.state.keys.toggle_piece(piece, from);
        self.state.keys.toggle_piece(promo_piece, to);
        self.update_castle_rights(from, to);
    }

    #[inline]
    fn do_capture_promo(&mut self, piece: Piece, from: Square, to: Square, promo_pt: PieceType) {
        let us = self.stm;
        // Reset fifty move counter
        self.state.fifty_move = 0;
        // Calculate the captured piece and the promoted piece
        let captured_piece = unsafe { self.on_unchecked(to) };
        let promo_piece = Piece::from_parts(us, promo_pt);
        // Update captured piece in board state
        self.state.captured = Some(captured_piece);
        // Move piece logic: Remove pawn and captured piece, and then add the promoted piece to the destination square
        self.remove_piece(to);
        self.remove_piece(from);
        self.add_piece(promo_piece, to);
        // Update hash keys
        self.state.keys.toggle_piece(promo_piece, to);
        self.state.keys.toggle_piece(captured_piece, to);
        self.state.keys.toggle_piece(piece, from);

        self.update_castle_rights(from, to);
    }

    /// Performs a castling move, handling king and rook piece movements.
    /// This is compatible with standard chess and Chess960 rules where rook and king
    /// positions might vary. The `from` and `to` squares refer to the king's movement.
    #[inline]
    fn do_castle(&mut self, from: Square, to: Square, flag: MoveFlag) {
        let us = self.stm;
        let rook = Piece::from_parts(us, PieceType::Rook);
        let king = self.on(from).unwrap();

        let is_king_castle = flag == MoveFlag::KingCastle;
        let rook_from = self.rook_from(is_king_castle);
        let rook_to = self.rook_to(is_king_castle);

        // Special move logic, have to pick up the king, move the rook then put the king back down (Chess960 moves can mean the king takes the spot of the rook,
        // which is why we cannot use two move piece functions)
        self.remove_piece(from);
        self.move_piece(rook_from, rook_to);
        self.add_piece(king, to);

        // Update main key
        self.state.keys.toggle_piece(king, from);
        self.state.keys.toggle_piece(king, to);
        self.state.keys.toggle_piece(rook, rook_from);
        self.state.keys.toggle_piece(rook, rook_to);

        // Update castling rights
        self.update_castle_rights(from, to);
    }

    /// Undo castling move
    #[inline]
    fn undo_castle(&mut self, from: Square, to: Square, flag: MoveFlag) {
        let us = self.stm;

        let is_king_castle = flag == MoveFlag::KingCastle;
        let rook_from = self.rook_from(is_king_castle);
        let rook_to = self.rook_to(is_king_castle);

        // Special move logic, have to pick up the king, move the rook then put the king back down (Chess960 moves can mean the king takes the spot of the rook,
        // which is why we cannot use two move piece functions)
        self.remove_piece(to);
        self.move_piece(rook_to, rook_from);
        self.add_piece(Piece::from_parts(us, PieceType::King), from);
    }

    /******************************************\
    |==========================================|
    |                 Make Move                |
    |==========================================|
    \******************************************/

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
        use MoveFlag::*;

        self.store_state();
        // Increment the half move counter (ply count)
        self.half_moves += 1;

        // Initialise variables
        let from = move_.from();
        let to = move_.to();
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
            QuietMove => self.do_quiet(piece, from, to),
            // Handle Double Pawn Pushes
            DoublePawnPush => self.do_double_pawn_push(piece, from, to),
            // Handle Castling
            KingCastle | QueenCastle => self.do_castle(from, to, flag),
            // Handle Capture
            Capture => self.do_capture(piece, from, to),
            // Handle Enpassant
            EPCapture => self.do_ep_capture(piece, from, to),
            // Promotion without capture
            KnightPromo | BishopPromo | RookPromo | QueenPromo => {
                let promo_pt = unsafe { move_.promotion_pt() };
                self.do_quiet_promo(piece, from, to, promo_pt);
            }
            // Promotion with capture
            KnightPromoCapture | BishopPromoCapture | RookPromoCapture | QueenPromoCapture => {
                let promo_pt = unsafe { move_.promotion_pt() };
                self.do_capture_promo(piece, from, to, promo_pt);
            }
        }

        // Toggle side to move *after* all other updates
        self.stm = !self.stm;
        // Update hash key for the change in side to move
        self.state.keys.toggle_side();
        // Update masks
        self.update_masks();
        // Update repetitions
        self.update_repetitions();
    }

    /// Applies a null move to the board.
    ///
    /// A null move is a pass; only the side to move changes, the en passant square
    /// (if any) is cleared, and the fifty-move counter is reset.
    /// Zobrist keys are updated accordingly.
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
        // Switch sides
        self.stm = !self.stm;

        self.update_masks();
    }

    /******************************************\
    |==========================================|
    |                 Undo Move                |
    |==========================================|
    \******************************************/

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
        use MoveFlag::*;
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

        // Restore state after extracting the useful information from the current state
        // Castling rights and enpassant squares are restored by the previous state
        self.restore_state();

        // Reverse the piece movements based on the move flag
        match flag {
            // Handle Quiet Moves and Double Pawn Pushes
            QuietMove | DoublePawnPush => {
                // Move the piece back from 'to' to 'from'
                self.move_piece(to, from);
                // En passant square (if any existed before this move) is restored by restore_state
            }
            // Handle Captures
            Capture => {
                // Move the attacking piece back
                self.move_piece(to, from);
                // Add the captured piece (retrieved from restored state) back to the 'to' square
                self.add_piece(unsafe { captured.unwrap_unchecked() }, to);
            }
            // Handle Enpassant
            EPCapture => {
                // Move the attacking pawn back
                self.move_piece(to, from);
                // Calculate the square where the EP captured pawn was
                let cap_sq = unsafe { to.add(-us.forward()).unwrap_unchecked() };
                // Add the captured pawn (always Pawn of opposite color) back
                self.add_piece(unsafe { captured.unwrap_unchecked() }, cap_sq);
            }
            // Handle Castling
            KingCastle | QueenCastle => {
                self.undo_castle(from, to, flag);
            }
            // Promotion without capture
            KnightPromo | BishopPromo | RookPromo | QueenPromo => {
                // Remove the promoted piece from the 'to' square
                self.remove_piece(to);
                // Add the original pawn back to the 'from' square
                self.add_piece(Piece::from_parts(us, PieceType::Pawn), from);
            }
            // Promotion with capture
            KnightPromoCapture | BishopPromoCapture | RookPromoCapture | QueenPromoCapture => {
                // Remove the promoted piece from the 'to' square
                self.remove_piece(to);
                // Add the captured piece (from restored state) back to the 'to' square
                self.add_piece(unsafe { captured.unwrap_unchecked() }, to);
                // Add the original pawn back to the 'from' square
                self.add_piece(Piece::from_parts(us, PieceType::Pawn), from);
            }
        }
    }

    /// Reverts a null move, restoring the board to its previous state.
    pub fn undo_null_move(&mut self) {
        // Switch sides
        self.stm = !self.stm;

        self.restore_state();
    }

    /******************************************\
    |==========================================|
    |              Legality check              |
    |==========================================|
    \******************************************/

    /// Checks if a given move is legal in the current board position.
    ///
    /// This function performs a series of checks to validate the move against
    /// chess rules, including:
    /// - Basic validity (e.g., moving own piece, not capturing own piece).
    /// - Piece-specific movement rules (pawn pushes, captures, EP, knight, bishop, rook, queen, king).
    /// - Castling legality (rights, path clear, not in check).
    /// - Ensuring the king is not left in check after the move (handles pins).
    pub fn is_legal(&self, move_: Move) -> bool {
        if !move_.is_valid() {
            return false;
        }

        let us = self.stm;

        let from = move_.from();
        let to = move_.to();
        let all_occ = self.all_occupied_bb();

        let captured = self.on(to);
        let is_pawn_move = move_.flag() == MoveFlag::DoublePawnPush
            || move_.is_promotion()
            || move_.flag() == MoveFlag::EPCapture;

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
            && move_.flag() != MoveFlag::EPCapture
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
            if move_.flag() == MoveFlag::EPCapture {
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
                if move_.flag() == MoveFlag::DoublePawnPush
                    && from.relative(us).rank() != Rank::Rank2
                {
                    return false;
                }
                // Quiet pawn moves
                let forward = us.forward();
                let push_sq = unsafe { from.add_unchecked(forward) };
                // The square in front should be clear
                if all_occ.contains(push_sq)
                    || (move_.flag() != MoveFlag::DoublePawnPush && push_sq != to)
                {
                    return false;
                }

                if move_.flag() == MoveFlag::DoublePawnPush {
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
                && !(move_.flag() == MoveFlag::EPCapture
                    && self.check_mask().contains(self.ep_target().unwrap()))
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

    /// Checks if a move is a capture by simply looking at the destination square.
    ///
    /// Note: This is a superficial check and does not validate full move legality
    /// or special capture types like en passant. It only checks if the destination
    /// square is occupied by an opponent's piece. For en passant, this will return false.
    pub fn is_capture(&self, move_: Move) -> bool {
        move_.is_valid() && self.on(move_.to()).is_some_and(|p| p.colour() != self.stm)
    }
}

/******************************************\
|==========================================|
|                Unit Tests                |
|==========================================|
\******************************************/

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
        } else if move_to_test.flag() == MoveFlag::EPCapture {
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
            Move::new(Square::B7, Square::B8, MoveFlag::QueenPromo),
            "rQbqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 6",
        );
    }

    #[test]
    fn test_promotion_capture() {
        let fen_before_promo_cap = "r1bqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6";
        test_make_undo(
            fen_before_promo_cap,
            Move::new(Square::B7, Square::A8, MoveFlag::KnightPromoCapture),
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
            Move::new(Square::B7, Square::B8, MoveFlag::QueenPromo),
            "rQbqkbnr/p1pppppp/8/8/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 6",
        );
    }

    #[test]
    fn test_promotion_capture_pawn_keys() {
        let fen_before_promo_cap = "rnbqkbnr/ppPppppp/8/8/8/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1";
        test_make_undo(
            fen_before_promo_cap,
            Move::new(Square::C7, Square::D8, MoveFlag::QueenPromoCapture),
            "rnbQkbnr/pp1ppppp/8/8/8/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1",
        );
    }

    #[test]
    fn test_promotion_capture_non_pawn_keys() {
        let fen_before_promo_cap = "rnbqkbnr/pPpppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 6";
        test_make_undo(
            fen_before_promo_cap,
            Move::new(Square::B7, Square::A8, MoveFlag::KnightPromoCapture),
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
