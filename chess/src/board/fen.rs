//! Provides FEN (Forsyth-Edwards Notation) parsing functionality for the `Board` struct.
//! This module includes the main `set` method to apply a FEN string to a board,
//! helper functions for parsing each FEN field, and common FEN string constants.

use thiserror::Error;

// Assuming Board is defined in src/core/board.rs which is the parent module
use super::Board;
// Import necessary core types (Piece, Square, Rank, File, Colour, Castling, etc.)
use super::movegen::pin_bb;
use crate::core::*;

/******************************************\
|==========================================|
|            Useful fen strings            |
|==========================================|
\******************************************/

/// FEN string for an empty board.
pub const EMPTY_FEN: &str = "r2k3r/8/8/8/8/8/8/R2K3R w KQkq - 0 1";
/// FEN string for the standard chess starting position.
pub const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
/// FEN string for a complex position often used for testing ("Tricky Position").
pub const TRICKY_FEN: &str = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
/// Fen string for a complex position often used for testing killer moves
pub const KILLER_FEN: &str = "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1";

/******************************************\
|==========================================|
|               Parse Fen                  |
|==========================================|
\******************************************/

/// Implementation block for FEN parsing methods on the `Board` struct.
impl Board {
    pub const FEN_SECTIONS: usize = 6;

    /// # Set Board State from FEN String
    ///
    /// Parses a FEN string and updates the board state (`self`) accordingly.
    ///
    /// **Note**: This function assumes the user wants to clear the entire board and restart.
    ///
    /// ## Arguments
    /// * `fen`: A string slice (`&str`) representing the FEN notation.
    ///
    /// ## Returns
    /// * `Ok(())`: If the FEN string was parsed and applied successfully.
    /// * `Err(FenParseError)`: If the FEN string is invalid or malformed.
    ///
    /// ## Errors
    /// Returns `FenParseError` if:
    /// * The FEN string does not have exactly 6 fields.
    /// * Any field contains invalid characters or formatting (e.g., invalid piece, rank format, castling char, etc.).
    /// * Numeric values (clocks) are out of range or unparsable.
    ///
    /// ## FEN Format Reminder
    /// `<Piece Placement> <Side to move> <Castling> <En passant> <Halfmove clock> <Fullmove counter>`
    pub fn set(&mut self, fen: &str) -> Result<(), FenParseError> {
        // Split the FEN string into its 6 components.
        let parts: Vec<&str> = fen.split_whitespace().take(6).collect();

        if parts.len() != Self::FEN_SECTIONS {
            return Err(FenParseError::InvalidNumberOfFields);
        }

        *self = Board::new();

        // --- 1. Parse Piece Placements ---

        let mut file: u8 = 0;
        let mut rank: u8 = 7;

        for token in parts[0].chars() {
            if file > 8 {
                return Err(FenParseError::InvalidRankFormat(
                    "Too many pieces on a file or the skip was too big".to_string(),
                ));
            }

            match token {
                '/' => {
                    if file != 8 {
                        return Err(FenParseError::InvalidRankFormat(
                            "Rank ended prematurely at file index (expected 8) before '/'"
                                .to_string(),
                        ));
                    }

                    file = 0;

                    if rank == 0 {
                        return Err(FenParseError::InvalidRankFormat(
                            "Too many rank separators ('/') found after completing rank"
                                .to_string(),
                        ));
                    }

                    rank -= 1;
                }
                '1'..='8' => {
                    file += token.to_digit(10).unwrap() as u8;
                }
                _ => {
                    if token.is_ascii_digit() {
                        return Err(FenParseError::InvalidRankFormat(format!(
                            "Invalid skip digit '{}' (must be 1-8)",
                            token,
                        )));
                    }

                    if file >= 8 {
                        return Err(FenParseError::InvalidRankFormat(format!(
                            "Piece placement '{}' attempted beyond file H (index >= 8) on rank {:?}",
                            token, rank
                        )));
                    }

                    // Justified as rank and file are constrained by checks above
                    let square = unsafe { Square::from_unchecked(rank * 8 + file) };
                    let piece = token
                        .to_string()
                        .parse::<Piece>()
                        .map_err(|_| FenParseError::InvalidPiecePlacementChar(token))?;
                    self.add_piece(piece, square);
                    file += 1;
                }
            }
        }

        // --- Final checks after processing all pieces ---

        if file != 8 || rank != 0 {
            return Err(FenParseError::InvalidRankFormat(
                "Fen string does not cover all 64 squares".to_string(),
            ));
        }

        // --- 2. Parse Side To Move ---
        match parts[1] {
            "w" => self.stm = Colour::White,
            "b" => self.stm = Colour::Black,
            _ => return Err(FenParseError::InvalidSideToMove(parts[1].to_string())),
        };

        // --- 3. Parse Castling Rights ---
        self.parse_castling(parts[2])?;

        // --- 4. Parse En Passant Square ---
        match parts[3] {
            "-" => self.state.enpassant = None,
            _ => {
                let ep_sq: Square = parts[3]
                    .parse()
                    .map_err(|_| FenParseError::InvalidEnPassantSquare(parts[3].to_string()))?;

                self.state.enpassant = Some(ep_sq);
                // Bounds checks
                if ep_sq.rank() != Rank::Rank3 && ep_sq.rank() != Rank::Rank6 {
                    return Err(FenParseError::InvalidEnPassantSquare(format!(
                        "{ep_sq} is not a valid enpassant square {:?}",
                        ep_sq.rank()
                    )));
                }
            }
        }

        // --- 5. Parse Halfmove Clock (Fifty-move rule counter) ---
        self.state.fifty_move = parts[4]
            .parse() // Parse directly into u8.
            .map_err(|_| FenParseError::InvalidHalfmoveClock(parts[4].to_string()))?;

        // --- 6. Parse Fullmove Number ---
        let full_move_number: u16 = parts[5]
            .parse() // Parse directly into u16.
            .map_err(|_| FenParseError::InvalidFullmoveNumber(parts[5].to_string()))?;

        // FEN fullmove number must be 1 or greater.
        if full_move_number == 0 {
            return Err(FenParseError::InvalidFullmoveNumber(format!(
                "Fullmove number cannot be 0, found: {}",
                parts[5]
            )));
        }

        // Calculate ply count (number of half-moves).
        // Ply = (Full Moves - 1) * 2 + (0 if White to move, 1 if Black to move)
        self.half_moves = (full_move_number - 1) * 2 + (self.stm() as u16); // Assumes Colour::White = 0, Colour::Black = 1

        // --- 8. Update Zobrist keys
        self.state.keys.key = self.calc_key();
        self.state.keys.pawn_key = self.calc_pawn_key();
        self.state.keys.non_pawn_key = self.calc_non_pawn_key();

        // --- 9. Update masks
        self.update_masks();

        Ok(())
    }

    /// # Board constructor using a fen string
    /// Creates a new `Board` instance from a FEN string.
    ///
    /// ## Arguments
    /// * `fen`: A string slice (`&str`) representing the FEN notation.
    ///
    /// ## Returns
    /// * `Ok(Board)`: If the FEN string was parsed and applied successfully.
    /// * `Err(FenParseError)`: If the FEN string is invalid or malformed.
    ///
    /// ## Errors
    /// Returns `FenParseError` if:
    /// * The FEN string does not have exactly 6 fields.
    /// * Any field contains invalid characters or formatting (e.g., invalid piece, rank format, castling char, etc.).
    /// * Numeric values (clocks) are out of range or unparsable.
    ///
    /// ## FEN Format Reminder
    /// `<Piece Placement> <Side to move> <Castling> <En passant> <Halfmove clock> <Fullmove counter>`
    pub fn from_fen(fen: &str) -> Result<Self, FenParseError> {
        let mut board = Board::new();
        board.set(fen)?;
        Ok(board)
    }

    /// # Get FEN String
    ///
    /// Generates a FEN (Forsyth-Edwards Notation) string representing the current
    /// state of the board.
    ///
    /// ## Returns
    /// * `String`: The FEN string representing the board state.
    ///
    /// ## Example
    ///
    /// ```
    /// use chess::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.fen(), START_FEN);
    /// ```
    pub fn fen(&self) -> String {
        let mut fen = String::new();

        // --- 1. Piece Placement ---
        for rank in Rank::iter().rev() {
            let mut empty_count = 0;
            for file in File::iter() {
                let square = Square::from_parts(file, rank);
                match self.on(square) {
                    Some(piece) => {
                        if empty_count > 0 {
                            fen.push_str(&empty_count.to_string());
                            empty_count = 0;
                        }
                        fen.push_str(&piece.to_string());
                    }
                    None => {
                        empty_count += 1;
                    }
                }
            }
            if empty_count > 0 {
                fen.push_str(&empty_count.to_string());
            }
            if rank != Rank::Rank1 {
                fen.push('/');
            }
        }

        // --- 2. Side to Move ---
        fen.push(' ');
        fen.push_str(match self.stm {
            Colour::White => "w",
            Colour::Black => "b",
        });

        // --- 3. Castling Rights ---
        let white_ksq = self.piece_bb(Colour::White, PieceType::King).lsb().unwrap();
        let black_ksq = self.piece_bb(Colour::Black, PieceType::King).lsb().unwrap();

        fen.push(' ');
        if self.state.castle == Castling::NONE {
            fen.push('-');
        } else {
            if self.state.castle.has(Castling::WK) {
                let wk_side_rooks =
                    self.piece_bb(Colour::White, PieceType::Rook) & pin_bb(white_ksq, Square::H1);
                if wk_side_rooks.count_bits() == 1 {
                    fen.push('K');
                } else {
                    let rook_sq = self.castling_mask.rook_sq[0]
                        .expect(" There should be a rook square set in Chess960 Positions");
                    fen.push(
                        rook_sq
                            .file()
                            .to_string()
                            .to_uppercase()
                            .chars()
                            .next()
                            .unwrap(),
                    );
                }
            }
            if self.state.castle.has(Castling::WQ) {
                let wq_side_rooks =
                    self.piece_bb(Colour::White, PieceType::Rook) & pin_bb(white_ksq, Square::A1);
                if wq_side_rooks.count_bits() == 1 {
                    fen.push('Q');
                } else {
                    let rook_sq = self.castling_mask.rook_sq[1]
                        .expect(" There should be a rook square set in Chess960 Positions");
                    fen.push(
                        rook_sq
                            .file()
                            .to_string()
                            .to_uppercase()
                            .chars()
                            .next()
                            .unwrap(),
                    );
                }
            }
            if self.state.castle.has(Castling::BK) {
                let bk_side_rooks =
                    self.piece_bb(Colour::Black, PieceType::Rook) & pin_bb(black_ksq, Square::H8);
                if bk_side_rooks.count_bits() == 1 {
                    fen.push('k');
                } else {
                    let rook_sq = self.castling_mask.rook_sq[2]
                        .expect(" There should be a rook square set in Chess960 Positions");
                    fen.push(rook_sq.file().to_string().chars().next().unwrap());
                }
            }
            if self.state.castle.has(Castling::BQ) {
                let bq_side_rooks =
                    self.piece_bb(Colour::Black, PieceType::Rook) & pin_bb(black_ksq, Square::A8);
                if bq_side_rooks.count_bits() == 1 {
                    fen.push('q');
                } else {
                    let rook_sq = self.castling_mask.rook_sq[3]
                        .expect(" There should be a rook square set in Chess960 Positions");
                    fen.push(rook_sq.file().to_string().chars().next().unwrap());
                }
            }
        }

        // --- 4. En Passant Square ---
        fen.push(' ');
        match self.state.enpassant {
            Some(square) => fen.push_str(&square.to_string()),
            None => fen.push('-'),
        }

        // --- 5. Halfmove
        fen.push_str(&format!(" {}", self.state.fifty_move));

        // --- 6. Fullmove Number ---
        fen.push_str(&format!(" {}", (self.half_moves / 2) + 1));

        fen
    }

    /// # Parse Castling Rights Field
    ///
    /// Parses the third field of the FEN string (e.g., "KQkq", "Kq", "-")
    /// and updates the board's castling rights state.
    ///
    /// ## Arguments
    /// * `self`: Mutable reference to the `Board`.
    /// * `castling`: The string slice for the third FEN field.
    ///
    /// ## Returns
    /// * `Ok(())`: If the castling string contains valid characters ('K', 'Q', 'k', 'q', '-').
    /// * `Err(FenParseError::InvalidCastlingChar)`: If an invalid character is encountered.
    fn parse_castling(&mut self, castling: &str) -> Result<(), FenParseError> {
        // Reset castling rights before applying the ones from FEN.
        self.state.castle = Castling::NONE;

        let white_ksq = self
            .piece_bb(Colour::White, PieceType::King)
            .pop_lsb()
            .ok_or(FenParseError::InvalidPosition(
                "There should be exactly one white king on the board".to_string(),
            ))?;
        let black_ksq = self
            .piece_bb(Colour::Black, PieceType::King)
            .pop_lsb()
            .ok_or(FenParseError::InvalidPosition(
                "There should be exactly one black king on the board".to_string(),
            ))?;

        self.castling_mask.castling[white_ksq.index()].remove(Castling::WHITE_CASTLING);
        self.castling_mask.castling[black_ksq.index()].remove(Castling::BLACK_CASTLING);

        // Handle the case of no castling rights available.
        if castling == "-" {
            return Ok(());
        }

        // Iterate through the characters and set the corresponding flags.
        for c in castling.chars() {
            match c {
                'K' => {
                    self.state.castle.set(Castling::WK);
                    let wk_side_rook = self.piece_bb(Colour::White, PieceType::Rook)
                        & pin_bb(white_ksq, Square::H1);
                    let rook_sq = wk_side_rook
                        .lsb()
                        .ok_or(FenParseError::InvalidPosition(
                "There should be exactly one king side white rook on the board when the castling flag K is set".to_string()))?;
                    self.castling_mask.rook_sq[0] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(Castling::WK);
                }
                'Q' => {
                    self.state.castle.set(Castling::WQ);
                    let wq_side_rook = self.piece_bb(Colour::White, PieceType::Rook)
                        & pin_bb(white_ksq, Square::A1);
                    let rook_sq = wq_side_rook
                    .lsb()
                    .ok_or(FenParseError::InvalidPosition(
                "There should be exactly one queen side white rook on the board when the castling flag Q is set".to_string()))?;
                    self.castling_mask.rook_sq[1] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(Castling::WQ);
                }
                'k' => {
                    self.state.castle.set(Castling::BK);
                    let bk_side_rook = self.piece_bb(Colour::Black, PieceType::Rook)
                        & pin_bb(black_ksq, Square::H8);

                    let rook_sq = bk_side_rook
                    .lsb()
                    .ok_or(FenParseError::InvalidPosition(
                "There should be exactly one king side black rook on the board when the castling flag k is set".to_string()))?;
                    self.castling_mask.rook_sq[2] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(Castling::BK);
                }
                'q' => {
                    self.state.castle.set(Castling::BQ);

                    let bq_side_rook = self.piece_bb(Colour::Black, PieceType::Rook)
                        & pin_bb(black_ksq, Square::A8);

                    let rook_sq = bq_side_rook
                    .lsb()
                    .ok_or(FenParseError::InvalidPosition(
                "There should be exactly one queen side black rook on the board when the castling flag q is set".to_string()))?;
                    self.castling_mask.rook_sq[3] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(Castling::BQ);
                }
                'A'..='H' | 'a'..='h' => {
                    let is_white = c.is_uppercase();
                    let ksq = if is_white { white_ksq } else { black_ksq };
                    let back_rank = if is_white { Rank::Rank1 } else { Rank::Rank8 };
                    let file = c.to_lowercase().to_string().parse::<File>().unwrap();
                    let is_king_side = file >= ksq.file();

                    let rook_sq = Square::from_parts(file, back_rank);

                    assert!(
                        self.on(rook_sq).map(|pc| pc.pt()) == Some(PieceType::Rook),
                        "{rook_sq}"
                    );

                    let index = 2 * !is_white as usize + !is_king_side as usize;

                    let rights = [Castling::WK, Castling::WQ, Castling::BK, Castling::BQ][index];

                    self.state.castle.set(rights);
                    self.castling_mask.rook_sq[index] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(rights);
                }

                // '-' is only valid if it's the *only* character, handled above.
                // Any other character is invalid.
                _ => return Err(FenParseError::InvalidCastlingChar(c)),
            };
        }

        Ok(())
    }
}

/******************************************\
|==========================================|
|             Fen Parse Errors             |
|==========================================|
\******************************************/

/// Represents errors that can occur during the parsing of a Forsyth–Edwards Notation (FEN) string.
///
/// A valid FEN string defines a particular board position, side to move, castling rights,
/// en passant target square, halfmove clock, and fullmove number, all separated by spaces.
/// Example: `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`
#[derive(Error, Debug, PartialEq, Eq, Clone)] // Added Clone for consistency if needed elsewhere
pub enum FenParseError {
    #[error("FEN string must have 6 fields separated by spaces")]
    InvalidNumberOfFields,
    #[error("Invalid character in FEN piece placement: '{0}'")]
    InvalidPiecePlacementChar(char),
    #[error("Invalid position: {0}")]
    InvalidPosition(String),
    #[error("Invalid rank format: {0}")]
    InvalidRankFormat(String),
    #[error("Invalid halfmove clock value: {0}")]
    InvalidHalfmoveClock(String),
    #[error("Invalid fullmove number value: {0}")]
    InvalidFullmoveNumber(String),
    #[error("Invalid en passant target square: {0}")]
    InvalidEnPassantSquare(String),
    #[error("Invalid side to move: {0}")]
    InvalidSideToMove(String),
    #[error("Invalid character in FEN castling availability: {0}")]
    InvalidCastlingChar(char),
}

// Add tests for FEN parsing
#[cfg(test)]
mod tests {
    // Import everything else from the parent module (fen.rs)
    use super::*;

    #[test]
    fn test_parse_start_fen() {
        let mut board = Board::new();
        assert!(board.set(START_FEN).is_ok());

        // Verify some key pieces and state using board methods if available
        // Assuming `on` gets piece at square and `state` gets BoardState ref
        assert_eq!(board.on(Square::A1), Some(Piece::WhiteRook));
        assert_eq!(board.on(Square::E1), Some(Piece::WhiteKing));
        assert_eq!(board.on(Square::H8), Some(Piece::BlackRook));
        assert_eq!(board.on(Square::D8), Some(Piece::BlackQueen));
        assert_eq!(board.on(Square::E4), None); // Check an empty square
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.state.castle, Castling::ALL);
        assert_eq!(board.state.enpassant, None);
        assert_eq!(board.state.fifty_move, 0); // Check parsed fifty_move
        assert_eq!(board.half_moves(), 0); // Check calculated ply count (startpos is ply 0)
        assert_eq!(board.fen(), START_FEN.trim());
    }

    #[test]
    fn test_parse_empty_fen() {
        let mut board = Board::new();
        assert!(board.set(EMPTY_FEN).is_ok());
        // Check bitboards directly if accessible, or use helper methods
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.state.castle, Castling::ALL); // FEN specifies KQkq
        assert_eq!(board.state.enpassant, None);
        assert_eq!(board.state.fifty_move, 0);
        assert_eq!(board.half_moves(), 0); // Empty board, move 1, white to move -> ply 0
        assert_eq!(board.fen(), EMPTY_FEN.trim());
    }

    #[test]
    fn test_parse_tricky_fen() {
        let mut board = Board::new();
        // Note: TRICKY_FEN has a trailing space, trim should handle it.
        assert!(board.set(TRICKY_FEN).is_ok());
        // Check some specific squares
        assert_eq!(board.on(Square::A8), Some(Piece::BlackRook));
        assert_eq!(board.on(Square::E8), Some(Piece::BlackKing));
        assert_eq!(board.on(Square::H8), Some(Piece::BlackRook));
        assert_eq!(board.on(Square::F3), Some(Piece::WhiteQueen));
        assert_eq!(board.on(Square::C3), Some(Piece::WhiteKnight));
        assert_eq!(board.on(Square::H3), Some(Piece::BlackPawn));
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.state.castle, Castling::ALL); // FEN specifies KQkq
        assert_eq!(board.state.enpassant, None); // FEN has '-', so None
        assert_eq!(board.state.fifty_move, 0);
        assert_eq!(board.half_moves(), 0); // Tricky pos, move 1, white to move -> ply 0
        assert_eq!(board.fen(), TRICKY_FEN.trim());
    }

    #[test]
    fn test_fen_invalid_piece() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/ppppxppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidPiecePlacementChar('x'))
        ));
    }

    #[test]
    fn test_fen_invalid_rank_length_short() {
        let mut board = Board::new();
        // Rank 7 has only 7 files specified (PPPPPPP) before '/'
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        // Check the specific error message if needed
    }

    #[test]
    fn test_fen_invalid_rank_length_short_at_end() {
        let mut board = Board::new();
        // Rank 1 has only 7 files specified (RNBQKBN) at the end of string
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBN w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
    }

    #[test]
    fn test_fen_invalid_rank_length_long_piece() {
        let mut board = Board::new();
        // Rank 7 has 9 files specified (PPPPPPPPP)
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("attempted beyond file H") // Error comes from parse_piece
        );
    }

    #[test]
    fn test_fen_invalid_rank_length_long_skip() {
        let mut board = Board::new();
        // Rank 7: P6P1 -> 1+6+1+1=9 files implied
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/P6P1/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
    }

    #[test]
    fn test_fen_invalid_skip_digit_zero() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppp0ppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
    }

    #[test]
    fn test_fen_invalid_skip_digit_nine() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppp9ppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
    }

    #[test]
    fn test_fen_too_many_ranks() {
        let mut board = Board::new();
        let fen = "8/8/8/8/8/8/8/8/8 w KQkq - 0 1"; // 9 ranks separated by '/'
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
    }

    #[test]
    fn test_fen_too_few_ranks() {
        let mut board = Board::new();
        let fen = "8/8/8/8/8/8/8 w KQkq - 0 1"; // Only 7 ranks specified
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
    }

    #[test]
    fn test_fen_missing_fields() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"; // Missing clocks
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidNumberOfFields) // Error from .next().ok_or in set()
        ));
    }

    #[test]
    fn test_fen_invalid_side() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1";
        assert!(matches!(board.set(fen), Err(FenParseError::InvalidSideToMove(s)) if s == "x"));
    }

    #[test]
    fn test_fen_invalid_castling() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQXkq - 0 1";
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidCastlingChar('X'))
        ));
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w K-q - 0 1"; // '-' only allowed alone
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidCastlingChar('-'))
        ));
    }

    #[test]
    fn test_fen_invalid_enpassant() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e9 0 1"; // Invalid square
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidEnPassantSquare(s)) if s == "e9")
        );
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq zz 0 1"; // Invalid format
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidEnPassantSquare(s)) if s == "zz")
        );
    }

    #[test]
    fn test_fen_invalid_halfmove() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - fifty 1"; // Not a number
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidHalfmoveClock(s)) if s == "fifty")
        );
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - -1 1"; // Negative number (fails u8 parse)
        assert!(matches!(board.set(fen), Err(FenParseError::InvalidHalfmoveClock(s)) if s == "-1"));
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 256 1"; // Too large for u8
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidHalfmoveClock(s)) if s == "256")
        );
    }

    #[test]
    fn test_fen_invalid_fullmove() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 zero"; // Not a number
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidFullmoveNumber(s)) if s == "zero")
        );
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"; // Must be >= 1
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidFullmoveNumber(s)) if s.contains("cannot be 0"))
        );
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 -5"; // Negative (fails u16 parse)
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidFullmoveNumber(s)) if s == "-5")
        );
    }

    // Test case for ply calculation
    #[test]
    fn test_fen_ply_calculation() {
        let mut board = Board::new();
        // Position after 1. e4, Black to move (move 1) -> ply 1
        let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 1);
        assert_eq!(board.stm(), Colour::Black);
        assert_eq!(board.fen(), fen.trim());

        // Position after 1. e4 c5, White to move (move 2) -> ply 2
        let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 2);
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.fen(), fen.trim());

        // Position after 10 moves (e.g. 9...Nc6), White to move (move 10) -> ply 18
        let fen = "r1bqkbnr/pp1ppppp/2n5/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 1 10";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 18);
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.fen(), fen.trim());

        // Position after 10 moves (e.g. 10. d4), Black to move (move 10) -> ply 19
        let fen = "r1bqkbnr/pp1ppppp/2n5/2p5/3PP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 10";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 19);
        assert_eq!(board.stm(), Colour::Black);
        assert_eq!(board.fen(), fen.trim());
    }
}

#[cfg(test)]
mod xfen_tests {
    use super::*;

    // Helper to create a board and check basic parsing success
    fn assert_fen_parses(fen: &str) -> Board {
        Board::from_fen(fen)
            .unwrap_or_else(|e| panic!("FEN failed to parse: '{}', Error: {}", fen, e))
    }

    // Helper to check castling rights flags
    fn assert_castling_flags(board: &Board, expected_flags: Castling) {
        assert_eq!(
            board.state.castle, expected_flags,
            "Expected castling flags {:?}, but got {:?}",
            expected_flags, board.state.castle
        );
    }

    // Helper to check castling mask for a specific square and right
    fn assert_mask_removes(board: &Board, sq: Square, right: Castling, should_remove: bool) {
        let has_right = board.castling_mask.castling[sq.index()].has(right);
        assert_ne!(
            has_right, should_remove,
            "Mask check failed for {:?} on {:?}: expected removal={}, has_right={}",
            right, sq, should_remove, has_right
        );
    }

    // Helper to check stored rook square
    fn assert_rook_sq(board: &Board, index: usize, expected_sq: Option<Square>) {
        // Use Option<Square> because rook_sq might not be set if the right doesn't exist
        let actual_sq = board.castling_mask.rook_sq[index];
        assert_eq!(
            actual_sq, expected_sq,
            "Expected rook_sq[{}] to be {:?}, but got {:?}",
            index, expected_sq, actual_sq
        );
    }

    // --- Test Cases ---

    #[test]
    fn test_xfen_white_castling_dg() {
        // King E1, Rooks D1, G1. Castling rights: D-file (WQ), G-file (WK)
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/3RKNR1 w DG - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::WQ | Castling::WK);

        let ksq = Square::E1;
        let d_rook_sq = Square::D1; // WQ side relative to E1
        let g_rook_sq = Square::G1; // WK side relative to E1

        // Check mask updates
        assert_mask_removes(&board, ksq, Castling::WHITE_CASTLING, true); // King removes all white
        assert_mask_removes(&board, d_rook_sq, Castling::WQ, true); // D-Rook removes WQ
        assert_mask_removes(&board, d_rook_sq, Castling::WK, false); // D-Rook does NOT remove WK
        assert_mask_removes(&board, g_rook_sq, Castling::WK, true); // G-Rook removes WK
        assert_mask_removes(&board, g_rook_sq, Castling::WQ, false); // G-Rook does NOT remove WQ

        // Check stored rook squares (Indices: 0=WK, 1=WQ, 2=BK, 3=BQ)
        assert_rook_sq(&board, 0, Some(g_rook_sq)); // WK -> G1
        assert_rook_sq(&board, 1, Some(d_rook_sq)); // WQ -> D1
        assert_rook_sq(&board, 2, None); // BK -> None
        assert_rook_sq(&board, 3, None); // BQ -> None
    }

    #[test]
    fn test_xfen_black_castling_cf() {
        // King E8, Rooks C8, F8. Castling rights: C-file (BQ), F-file (BK)
        let fen = "2r1krn1/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b cf - 0 1"; // Note: case matters for XFEN rights
        let board = assert_fen_parses(fen);
        // The parser converts 'C' and 'F' to lowercase internally if side is black
        assert_castling_flags(&board, Castling::BQ | Castling::BK);

        let ksq = Square::E8;
        let c_rook_sq = Square::C8; // BQ side relative to E8
        let f_rook_sq = Square::F8; // BK side relative to E8

        // Check mask updates
        assert_mask_removes(&board, ksq, Castling::BLACK_CASTLING, true); // King removes all black
        assert_mask_removes(&board, c_rook_sq, Castling::BQ, true); // C-Rook removes BQ
        assert_mask_removes(&board, c_rook_sq, Castling::BK, false); // C-Rook does NOT remove BK
        assert_mask_removes(&board, f_rook_sq, Castling::BK, true); // F-Rook removes BK
        assert_mask_removes(&board, f_rook_sq, Castling::BQ, false); // F-Rook does NOT remove BQ

        // Check stored rook squares (Indices: 0=WK, 1=WQ, 2=BK, 3=BQ)
        assert_rook_sq(&board, 0, None); // WK -> None
        assert_rook_sq(&board, 1, None); // WQ -> None
        assert_rook_sq(&board, 2, Some(f_rook_sq)); // BK -> F8
        assert_rook_sq(&board, 3, Some(c_rook_sq)); // BQ -> C8
    }

    #[test]
    fn test_xfen_mixed_castling_bgcf() {
        // White: King E1, Rooks B1, G1 (BG -> WQ, WK)
        // Black: King E8, Rooks C8, F8 (cf -> BQ, BK)
        let fen = "2r1krn1/pppppppp/8/8/8/8/PPPPPPPP/1R2KNR1 w BGcf - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::ALL); // Should have all four rights

        let wk_sq = Square::E1;
        let bk_sq = Square::E8;
        let w_b_rook = Square::B1; // WQ
        let w_g_rook = Square::G1; // WK
        let b_c_rook = Square::C8; // BQ
        let b_f_rook = Square::F8; // BK

        // White Masks
        assert_mask_removes(&board, wk_sq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, w_b_rook, Castling::WQ, true);
        assert_mask_removes(&board, w_b_rook, Castling::WK, false);
        assert_mask_removes(&board, w_g_rook, Castling::WK, true);
        assert_mask_removes(&board, w_g_rook, Castling::WQ, false);

        // Black Masks
        assert_mask_removes(&board, bk_sq, Castling::BLACK_CASTLING, true);
        assert_mask_removes(&board, b_c_rook, Castling::BQ, true);
        assert_mask_removes(&board, b_c_rook, Castling::BK, false);
        assert_mask_removes(&board, b_f_rook, Castling::BK, true);
        assert_mask_removes(&board, b_f_rook, Castling::BQ, false);

        // Stored Rook Squares
        assert_rook_sq(&board, 0, Some(w_g_rook)); // WK -> G1
        assert_rook_sq(&board, 1, Some(w_b_rook)); // WQ -> B1
        assert_rook_sq(&board, 2, Some(b_f_rook)); // BK -> F8
        assert_rook_sq(&board, 3, Some(b_c_rook)); // BQ -> C8
    }

    #[test]
    fn test_xfen_king_on_corner_white_ah() {
        let fen_sp4 = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/1NBQKBRR w H - 0 1"; // K=G1, R=F1, R=H1
        let board = assert_fen_parses(fen_sp4);
        assert_castling_flags(&board, Castling::WK); // F -> WQ, H -> WK relative to G1

        let ksq = Square::E1;
        let h_rook_sq = Square::H1; // WK side relative to G1

        assert_mask_removes(&board, ksq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, h_rook_sq, Castling::WK, true);
        assert_mask_removes(&board, h_rook_sq, Castling::WQ, false);

        assert_rook_sq(&board, 0, Some(h_rook_sq)); // WK -> H1
    }

    #[test]
    fn test_xfen_custom_position() {
        let fen_sp512 = "rn2k1r1/ppp1pp1p/3p2p1/5bn1/P7/2N2B2/1PPPPP2/2BNK1RR w Gkq - 4 11";
        let board = assert_fen_parses(fen_sp512);
        assert_castling_flags(&board, Castling::WK | Castling::BK | Castling::BQ);
        assert_mask_removes(&board, Square::E8, Castling::BLACK_CASTLING, true);
        assert_mask_removes(&board, Square::G8, Castling::BK, true);
        assert_mask_removes(&board, Square::A8, Castling::BQ, true);
        assert_rook_sq(&board, 0, Some(Square::G1));
        assert_rook_sq(&board, 1, None);
        assert_rook_sq(&board, 2, Some(Square::G8));
        assert_rook_sq(&board, 3, Some(Square::A8));
    }

    #[test]
    fn test_xfen_no_castling_dash() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::NONE);

        // Check masks (kings still remove rights, rooks don't matter)
        let wk_sq = Square::E1;
        let bk_sq = Square::E8;
        assert_mask_removes(&board, wk_sq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, bk_sq, Castling::BLACK_CASTLING, true);

        // Check stored rook squares are empty
        assert_rook_sq(&board, 0, None);
        assert_rook_sq(&board, 1, None);
        assert_rook_sq(&board, 2, None);
        assert_rook_sq(&board, 3, None);
    }

    #[test]
    fn test_xfen_only_one_white_right_h() {
        // King E1, Rooks A1, H1. Castling: H (WK)
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w H - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::WK);

        let ksq = Square::E1;
        let h_rook_sq = Square::H1; // WK side relative to E1
        let a_rook_sq = Square::A1; // No rights for this one

        assert_mask_removes(&board, ksq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, h_rook_sq, Castling::WK, true);
        assert_mask_removes(&board, h_rook_sq, Castling::WQ, false);
        // Check A1 rook mask - should not be affected as 'A' right wasn't given
        assert_mask_removes(&board, a_rook_sq, Castling::WQ, false);
        assert_mask_removes(&board, a_rook_sq, Castling::WK, false);

        assert_rook_sq(&board, 0, Some(h_rook_sq)); // WK -> H1
        assert_rook_sq(&board, 1, None); // WQ -> None
    }

    #[test]
    fn test_xfen_invalid_char() {
        // Invalid char 'X'
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KX - 0 1";
        let result = Board::from_fen(fen);
        assert!(matches!(
            result,
            Err(FenParseError::InvalidCastlingChar('X'))
        ));

        // Invalid char '1'
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Q1 - 0 1";
        let result = Board::from_fen(fen);
        assert!(matches!(
            result,
            Err(FenParseError::InvalidCastlingChar('1'))
        ));
    }

    #[test]
    fn test_xfen_rook_not_present() {
        // Castling right 'H' given, but no rook on H1
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3KBN1 w H - 0 1"; // Rook missing on H1
        // The current code uses .expect() when finding the rook. This should panic.
        let result = std::panic::catch_unwind(|| Board::from_fen(fen));
        assert!(
            result.is_err(),
            "Parsing FEN with missing rook for castling right should panic"
        );
        // TODO: Ideally, this should return a FenParseError instead of panicking.
        // If the code is changed to return an error:
        // let result = Board::from_fen(fen);
        // assert!(matches!(result, Err(FenParseError::MissingCastlingRook(...))));
    }

    // Test standard FEN still works
    #[test]
    fn test_standard_fen_kqkq() {
        let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::ALL);

        let wk_sq = Square::E1;
        let bk_sq = Square::E8;
        let w_h_rook = Square::H1; // WK
        let w_a_rook = Square::A1; // WQ
        let b_h_rook = Square::H8; // BK
        let b_a_rook = Square::A8; // BQ

        // White Masks
        assert_mask_removes(&board, wk_sq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, w_a_rook, Castling::WQ, true);
        assert_mask_removes(&board, w_h_rook, Castling::WK, true);

        // Black Masks
        assert_mask_removes(&board, bk_sq, Castling::BLACK_CASTLING, true);
        assert_mask_removes(&board, b_a_rook, Castling::BQ, true);
        assert_mask_removes(&board, b_h_rook, Castling::BK, true);

        // Stored Rook Squares
        assert_rook_sq(&board, 0, Some(w_h_rook)); // WK -> H1
        assert_rook_sq(&board, 1, Some(w_a_rook)); // WQ -> A1
        assert_rook_sq(&board, 2, Some(b_h_rook)); // BK -> H8
        assert_rook_sq(&board, 3, Some(b_a_rook)); // BQ -> A8
    }
}
