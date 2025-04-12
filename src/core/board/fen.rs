//! Provides FEN (Forsyth-Edwards Notation) parsing functionality for the `Board` struct.
//! This module includes the main `set` method to apply a FEN string to a board,
//! helper functions for parsing each FEN field, and common FEN string constants.

// Assuming Board is defined in src/core/board.rs which is the parent module
use super::Board;
// Assuming FenParseError is defined in src/core/errors.rs
use super::errors::FenParseError;
// Import necessary core types (Piece, Square, Rank, File, Colour, Castling, etc.)
use crate::core::*;

/******************************************\
|==========================================|
|            Useful fen strings            |
|==========================================|
\******************************************/

/// FEN string for an empty board.
pub const EMPTY_FEN: &str = "8/8/8/8/8/8/8/8 w KQkq - 0 1";
/// FEN string for the standard chess starting position.
pub const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
/// FEN string for a complex position often used for testing ("Tricky Position").
pub const TRICKY_FEN: &str = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";

/******************************************\
|==========================================|
|             Fen Error Types              |
|==========================================|
\******************************************/
// FenParseError enum is defined in src/core/errors.rs

/******************************************\
|==========================================|
|               Parse Fen                  |
|==========================================|
\******************************************/

/// Implementation block for FEN parsing methods on the `Board` struct.
impl Board {
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
        *self = Board::new();

        // Split the FEN string into its 6 components.
        let mut parts = fen.trim().split_whitespace();

        // --- 1. Parse Piece Placement ---
        let piece_placement = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        // Note: This clears pieces implicitly via add_piece if the board wasn't empty.
        // Consider explicitly clearing the board's piece arrays/bitboards first if needed.
        self.parse_piece_placement(piece_placement)?;

        // --- 2. Parse Side to Move ---
        let side_to_move = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        self.parse_side_to_move(side_to_move)?;

        // --- 3. Parse Castling Rights ---
        let castling = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        self.parse_castling(castling)?;

        // --- 4. Parse En Passant Square ---
        let enpassant = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        self.parse_enpassant(enpassant)?;

        // --- 5. Parse Halfmove Clock (Fifty-move rule counter) ---
        let fifty_move_token = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        // The parse function returns the value, which we assign to the board state.
        self.state.fifty_move = self.parse_fifty_move(fifty_move_token)?;

        // --- 6. Parse Fullmove Number ---
        let full_move_token = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        // The parse function returns the calculated ply count, which we assign.
        // Note: Board stores ply count (half_moves), not the FEN fullmove number directly.
        self.half_moves = self.parse_full_move(full_move_token)?;

        // --- Final Check: Ensure no extra fields ---
        if parts.next().is_some() {
            return Err(FenParseError::InvalidNumberOfFields);
        }

        // TODO: Update Zobrist keys (self.state.key, self.state.pawn_key) if using them.
        // TODO: Update derived data like attack masks, king squares etc. if needed.

        Ok(())
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
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.fen(), START_FEN);
    /// ```
    pub fn fen(&self) -> String {
        let mut fen = String::new();

        // --- 1. Piece Placement ---
        for rank in Rank::iter().rev() {
            let mut empty_count = 0;
            for file in File::iter() {
                let square = Square::from((file, rank));
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
        fen.push_str(match self.side_to_move {
            Colour::White => "w",
            Colour::Black => "b",
        });

        // --- 3. Castling Rights ---
        fen.push(' ');
        if self.state.castle == Castling::NONE {
            fen.push('-');
        } else {
            if self.state.castle.has(Castling::WK) {
                fen.push('K');
            }
            if self.state.castle.has(Castling::WQ) {
                fen.push('Q');
            }
            if self.state.castle.has(Castling::BK) {
                fen.push('k');
            }
            if self.state.castle.has(Castling::BQ) {
                fen.push('q');
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

    /// # Parse Rank Separator ('/')
    ///
    /// Handles the '/' character found in the piece placement section of FEN.
    /// Validates that the previous rank was completed correctly and advances
    /// the rank iterator to the next rank (downwards).
    ///
    /// ## Arguments
    /// * `rank_iter`: A mutable reference to the iterator over `Rank`s (expected to be reversed, 8->1).
    /// * `rank`: The `Rank` that was just completed *before* encountering the '/'.
    /// * `file`: The file index reached on the completed `rank` (should be 8).
    ///
    /// ## Returns
    /// * `Ok((Rank, u8))`: A tuple containing the *next* `Rank` to process and the reset file index (0).
    /// * `Err(FenParseError)`: If the previous rank was incomplete (`file != 8`) or if there are too many '/' separators.
    fn parse_seperator(
        rank_iter: &mut impl DoubleEndedIterator<Item = Rank>,
        rank: Rank,
        file: u8,
    ) -> Result<(Rank, u8), FenParseError> {
        // Check if the rank ended exactly at the 8th file position.
        if file != 8 {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Rank {:?} ended prematurely at file index {} (expected 8) before '/'",
                rank, file
            )));
        }

        // Advance to the next rank (e.g., from Rank 8 to Rank 7).
        // Error if the iterator is exhausted (meaning more than 7 '/' were found).
        let next_rank = rank_iter.next().ok_or_else(|| {
            FenParseError::InvalidRankFormat(format!(
                "Too many rank separators ('/') found after completing rank {:?}",
                rank
            ))
        })?;

        // Return the next rank and reset the file index to 0 (File A).
        Ok((next_rank, 0))
    }

    /// # Parse Skip Digit ('1'-'8')
    ///
    /// Parses a digit character representing a number of consecutive empty squares.
    /// Validates the digit and ensures the skip doesn't exceed the rank boundary.
    ///
    /// ## Arguments
    /// * `skip`: The digit character (e.g., '3').
    /// * `idx`: The character index in the FEN string (for error reporting).
    /// * `current_rank`: The rank currently being parsed.
    /// * `current_file_index`: The file index *before* processing this skip digit.
    ///
    /// ## Returns
    /// * `Ok(u8)`: The number of files to skip (1 to 8).
    /// * `Err(FenParseError)`: If the digit is invalid ('0' or '9') or the skip goes past File H.
    fn parse_skip(
        skip: char,
        idx: usize,
        current_rank: Rank,
        current_file_index: u8,
    ) -> Result<u8, FenParseError> {
        // Convert char '1'..'8' to integer 1..8. .unwrap() is safe after is_digit check.
        let skip_val = skip.to_digit(10).unwrap();

        // FEN spec only allows digits 1 through 8 for skips.
        if !(1..=8).contains(&skip_val) {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Invalid skip digit '{}' (must be 1-8) at char index {}",
                skip,
                idx // Use original char for error message
            )));
        }

        let skip_u8 = skip_val as u8;
        // Check if adding the skip value exceeds the 8 files of a rank.
        if current_file_index + skip_u8 > 8 {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Skip value {} exceeds rank length at file index {} on rank {:?}",
                skip_u8, current_file_index, current_rank
            )));
        }
        // Return the valid skip amount.
        Ok(skip_u8)
    }

    /// # Parse Piece Character
    ///
    /// Parses a piece character (e.g., 'P', 'n', 'K', 'q') and places it on the board
    /// at the specified rank and file index.
    ///
    /// ## Arguments
    /// * `self`: Mutable reference to the `Board` to place the piece on.
    /// * `piece`: The character representing the piece.
    /// * `rank`: The `Rank` where the piece should be placed.
    /// * `file`: The file index (0-7) where the piece should be placed.
    ///
    /// ## Returns
    /// * `Ok(())`: If the piece was valid and placed successfully.
    /// * `Err(FenParseError)`: If the file index is out of bounds (>= 8) or the piece character is invalid.
    fn parse_piece(&mut self, piece: char, rank: Rank, file: u8) -> Result<(), FenParseError> {
        // Ensure we are not trying to place a piece beyond the H file.
        if file >= 8 {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Piece placement '{}' attempted beyond file H (index >= 8) on rank {:?}",
                piece, rank
            )));
        }

        // Attempt to parse the character into a `Piece` enum variant.
        let piece_enum = piece
            .to_string() // `parse` expects a &str
            .parse::<Piece>()
            // Map the specific ParsePieceError to our FenParseError type.
            .map_err(|_| FenParseError::InvalidPiecePlacementChar(piece))?;

        // Convert the file index (0-7) into a `File` enum variant (FileA-FileH).
        // File::from(u8) should handle this conversion.
        let current_file = File::from(file); // Assumes File::from(0..=7) is safe.

        // Create the `Square` from the `File` and `Rank`.
        let sq = Square::from((current_file, rank));

        // Add the parsed piece to the board at the calculated square.
        // This assumes `add_piece` handles updating internal board representations.
        self.add_piece(piece_enum, sq);

        Ok(())
    }

    /// # Parse Piece Placement Field
    ///
    /// Parses the first field of the FEN string, which describes the placement
    /// of pieces on the board. Iterates through ranks from 8 down to 1, and
    /// files from A to H within each rank.
    ///
    /// ## Arguments
    /// * `self`: Mutable reference to the `Board`.
    /// * `piece_placement`: The string slice corresponding to the first FEN field.
    ///
    /// ## Returns
    /// * `Ok(())`: If the piece placement string is valid and pieces are placed.
    /// * `Err(FenParseError)`: If the format is invalid (incorrect rank/file counts, invalid characters).
    fn parse_piece_placement(&mut self, piece_placement: &str) -> Result<(), FenParseError> {
        // Iterate ranks from 8 down to 1.
        let mut rank_iter = Rank::iter().rev();
        // Start with the first rank (Rank 8). Error if Rank::iter() is empty (impossible).
        let mut rank = rank_iter
            .next()
            .ok_or_else(|| FenParseError::InvalidRankFormat("Board has no ranks?".to_string()))?;
        // Initialize file index for the current rank (0 = File A).
        let mut file: u8 = 0;

        // Process each character in the piece placement string.
        for (i, char) in piece_placement.chars().enumerate() {
            match char {
                // Rank separator: Validate previous rank, move to next rank, reset file index.
                '/' => {
                    (rank, file) = Self::parse_seperator(&mut rank_iter, rank, file)?;
                }
                // Skip digit: Parse the digit, validate, and advance the file index.
                skip if skip.is_digit(10) => {
                    file += Self::parse_skip(skip, i, rank, file)?;
                }
                // Piece character: Parse the piece, place it, and advance the file index by 1.
                piece_char => {
                    self.parse_piece(piece_char, rank, file)?;
                    file += 1;
                }
            }
        }

        // --- Final checks after processing all characters ---

        // 1. Check if the *last* rank processed was fully completed.
        if file != 8 {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Final rank {:?} ended prematurely at file index {} (expected 8)",
                rank, file
            )));
        }

        // 2. Check if we processed exactly 8 ranks (the rank iterator should be empty).
        if rank_iter.next().is_some() {
            // This means we finished parsing the string, but the iterator still had ranks left (e.g., FEN was "8/8/8/8/8/8/8")
            // The error should indicate not enough ranks were *specified* in the string.
            return Err(FenParseError::InvalidRankFormat(
                "Not enough ranks specified in FEN string (expected 8)".to_string(),
            ));
        }
        // Conversely, if the loop finished *and* the iterator is empty *and* the last rank was file=8, it's valid.

        Ok(())
    }

    /// # Parse Side to Move Field
    ///
    /// Parses the second field of the FEN string ('w' or 'b') and updates the board state.
    ///
    /// ## Arguments
    /// * `self`: Mutable reference to the `Board`.
    /// * `side_to_move`: The string slice for the second FEN field.
    ///
    /// ## Returns
    /// * `Ok(())`: If the side to move is valid ('w' or 'b').
    /// * `Err(FenParseError::InvalidSideToMove)`: If the string is not 'w' or 'b'.
    fn parse_side_to_move(&mut self, side_to_move: &str) -> Result<(), FenParseError> {
        match side_to_move {
            "w" => self.side_to_move = Colour::White,
            "b" => self.side_to_move = Colour::Black,
            _ => return Err(FenParseError::InvalidSideToMove(side_to_move.to_string())),
        };
        Ok(())
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

        // Handle the case of no castling rights available.
        if castling == "-" {
            return Ok(());
        }

        // Iterate through the characters and set the corresponding flags.
        for c in castling.chars() {
            match c {
                'K' => self.state.castle.set(Castling::WK),
                'Q' => self.state.castle.set(Castling::WQ),
                'k' => self.state.castle.set(Castling::BK), // Note: FEN uses 'k'/'q' for black
                'q' => self.state.castle.set(Castling::BQ),
                // '-' is only valid if it's the *only* character, handled above.
                // Any other character is invalid.
                _ => return Err(FenParseError::InvalidCastlingChar(c)),
            };
        }

        Ok(())
    }

    /// # Parse En Passant Target Square Field
    ///
    /// Parses the fourth field of the FEN string (e.g., "e3", "h6", "-")
    /// and updates the board's en passant state.
    ///
    /// ## Arguments
    /// * `self`: Mutable reference to the `Board`.
    /// * `enpassant`: The string slice for the fourth FEN field.
    ///
    /// ## Returns
    /// * `Ok(())`: If the en passant string is valid ("-" or a valid square coordinate).
    /// * `Err(FenParseError::InvalidEnPassantSquare)`: If the string is not "-" and cannot be parsed as a `Square`.
    fn parse_enpassant(&mut self, enpassant: &str) -> Result<(), FenParseError> {
        self.state.enpassant = match enpassant {
            // No en passant target square.
            "-" => None,
            // Attempt to parse the string as a square coordinate.
            _ => Some(
                enpassant
                    .parse::<Square>() // Assumes Square implements FromStr correctly.
                    .map_err(|_| FenParseError::InvalidEnPassantSquare(enpassant.to_string()))?,
                // TODO: Add validation? (e.g., must be rank 3 or 6)
            ),
        };
        Ok(())
    }

    /// # Parse Halfmove Clock Field (Fifty-Move Rule)
    ///
    /// Parses the fifth field of the FEN string (a non-negative integer)
    /// representing the number of half-moves since the last capture or pawn advance.
    ///
    /// ## Arguments
    /// * `self`: Mutable reference to the `Board` (though not directly used here, kept for consistency).
    /// * `fifty_move_token`: The string slice for the fifth FEN field.
    ///
    /// ## Returns
    /// * `Ok(u8)`: The parsed halfmove clock value.
    /// * `Err(FenParseError::InvalidHalfmoveClock)`: If the string is not a valid non-negative `u8` integer.
    fn parse_fifty_move(&mut self, fifty_move_token: &str) -> Result<u8, FenParseError> {
        fifty_move_token
            .parse::<u8>() // Parse directly into u8.
            .map_err(|_| FenParseError::InvalidHalfmoveClock(fifty_move_token.to_string()))
    }

    /// # Parse Fullmove Number Field
    ///
    /// Parses the sixth field of the FEN string (a positive integer)
    /// representing the number of the full move. It starts at 1 and increments
    /// after Black's move. This function converts it to a ply count (number of half-moves
    /// since the start of the game) based on the already parsed side to move.
    ///
    /// ## Arguments
    /// * `self`: Mutable reference to the `Board` (used to get `side_to_move`).
    /// * `full_move_token`: The string slice for the sixth FEN field.
    ///
    /// ## Returns
    /// * `Ok(u16)`: The calculated ply count.
    /// * `Err(FenParseError::InvalidFullmoveNumber)`: If the string is not a valid positive `u16` integer.
    fn parse_full_move(&mut self, full_move_token: &str) -> Result<u16, FenParseError> {
        // Parse the fullmove number from the FEN string.
        let full_move_number = full_move_token
            .parse::<u16>()
            .map_err(|_| FenParseError::InvalidFullmoveNumber(full_move_token.to_string()))?;

        // FEN fullmove number must be 1 or greater.
        if full_move_number == 0 {
            return Err(FenParseError::InvalidFullmoveNumber(format!(
                "Fullmove number cannot be 0, found: {}",
                full_move_token
            )));
        }

        // Calculate ply count (number of half-moves).
        // Ply = (Full Moves - 1) * 2 + (0 if White to move, 1 if Black to move)
        let ply = (full_move_number - 1) * 2 + (self.side_to_move() as u16); // Assumes Colour::White = 0, Colour::Black = 1

        Ok(ply)
    }
}

// Add tests for FEN parsing
#[cfg(test)]
mod tests {
    // Import errors specifically if needed for matching
    use super::errors::FenParseError;
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
        assert_eq!(board.side_to_move(), Colour::White);
        assert_eq!(board.state().castle, Castling::ALL);
        assert_eq!(board.state().enpassant, None);
        assert_eq!(board.state().fifty_move, 0); // Check parsed fifty_move
        assert_eq!(board.half_moves(), 0); // Check calculated ply count (startpos is ply 0)
        assert_eq!(board.fen(), START_FEN.trim());
    }

    #[test]
    fn test_parse_empty_fen() {
        let mut board = Board::new();
        assert!(board.set(EMPTY_FEN).is_ok());
        // Check bitboards directly if accessible, or use helper methods
        assert!(board.occupied_bb(Colour::White).is_empty());
        assert!(board.occupied_bb(Colour::Black).is_empty());
        assert_eq!(board.side_to_move(), Colour::White);
        assert_eq!(board.state().castle, Castling::ALL); // FEN specifies KQkq
        assert_eq!(board.state().enpassant, None);
        assert_eq!(board.state().fifty_move, 0);
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
        assert_eq!(board.side_to_move(), Colour::White);
        assert_eq!(board.state().castle, Castling::ALL); // FEN specifies KQkq
        assert_eq!(board.state().enpassant, None); // FEN has '-', so None
        assert_eq!(board.state().fifty_move, 0);
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
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("ended prematurely at file index 7")
        );
    }

    #[test]
    fn test_fen_invalid_rank_length_short_at_end() {
        let mut board = Board::new();
        // Rank 1 has only 7 files specified (RNBQKBN) at the end of string
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBN w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Final rank Rank1 ended prematurely at file index 7")
        );
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
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Skip value 1 exceeds rank length") // Error comes from parse_skip
        );
    }

    #[test]
    fn test_fen_invalid_skip_digit_zero() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppp0ppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Invalid skip digit '0'") // Error from parse_skip
        );
    }

    #[test]
    fn test_fen_invalid_skip_digit_nine() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppp9ppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Invalid skip digit '9'") // Error from parse_skip
        );
    }

    #[test]
    fn test_fen_too_many_ranks() {
        let mut board = Board::new();
        let fen = "8/8/8/8/8/8/8/8/8 w KQkq - 0 1"; // 9 ranks separated by '/'
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Too many rank separators") // Error from parse_seperator
        );
    }

    #[test]
    fn test_fen_too_few_ranks() {
        let mut board = Board::new();
        let fen = "8/8/8/8/8/8/8 w KQkq - 0 1"; // Only 7 ranks specified
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Not enough ranks specified") // Error from final check in parse_piece_placement
        );
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
    fn test_fen_extra_fields() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 extra";
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidNumberOfFields) // Error from final check in set()
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
        assert_eq!(board.side_to_move(), Colour::Black);
        assert_eq!(board.fen(), fen.trim());

        // Position after 1. e4 c5, White to move (move 2) -> ply 2
        let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 2);
        assert_eq!(board.side_to_move(), Colour::White);
        assert_eq!(board.fen(), fen.trim());

        // Position after 10 moves (e.g. 9...Nc6), White to move (move 10) -> ply 18
        let fen = "r1bqkbnr/pp1ppppp/2n5/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 1 10";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 18);
        assert_eq!(board.side_to_move(), Colour::White);
        assert_eq!(board.fen(), fen.trim());

        // Position after 10 moves (e.g. 10. d4), Black to move (move 10) -> ply 19
        let fen = "r1bqkbnr/pp1ppppp/2n5/2p5/3PP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 10";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 19);
        assert_eq!(board.side_to_move(), Colour::Black);
        assert_eq!(board.fen(), fen.trim());
    }
}
