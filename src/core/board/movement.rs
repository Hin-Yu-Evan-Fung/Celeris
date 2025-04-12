use super::Board;
use crate::core::*;

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
        self.pieces[piece.piecetype() as usize].set(square);
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
        self.pieces[piece.piecetype() as usize].clear(square);
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

        let from_to_bb = Bitboard::from(from) | Bitboard::from(to);
        // Update piece bitboards
        self.pieces[piece.piecetype() as usize] ^= from_to_bb;
        // Update occupied bitboards
        self.occupied[piece.colour() as usize] ^= from_to_bb;
    }
}
