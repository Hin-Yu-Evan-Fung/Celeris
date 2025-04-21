//! Defines the custom error types used throughout the Celeris chess engine.
//!
//! These errors cover issues related to parsing core types (like pieces, squares, FEN strings),
//! performing board operations (like moving pieces off the board), and other potential
//! failure conditions within the engine's logic. Each error type provides specific
//! information about the cause of the failure.

// Add this attribute to enforce documentation on all public items
#![deny(missing_docs)]

use std::fmt; // Import fmt for use in doc examples if needed

/******************************************\
|==========================================|
|            Piece Parse Error             |
|==========================================|
\******************************************/

/// Represents errors that can occur when attempting to parse a [`Piece`](crate::core::Piece) from a string.
///
/// Typically expects a single character string corresponding to FEN notation (e.g., 'P', 'n', 'K').
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsePieceError {
    /// The input string did not have the expected length (usually 1 character).
    /// Contains the actual length received.
    ///
    /// # Example
    /// ```
    /// # use chess::core::errors::ParsePieceError;
    /// # use chess::core::Piece;
    /// let result = "Pn".parse::<Piece>();
    /// assert!(matches!(result, Err(ParsePieceError::InvalidLength(2))));
    /// ```
    InvalidLength(usize),

    /// The character provided in the input string is not a valid FEN representation
    /// of any chess piece (e.g., 'x', '1', ' ').
    /// Contains the invalid character.
    ///
    /// # Example
    /// ```
    /// # use chess::core::errors::ParsePieceError; // Corrected path
    /// # use chess::core::Piece; // Assuming Piece is in core
    /// let result = "X".parse::<Piece>(); // 'X' is not a valid piece character
    /// assert!(matches!(result, Err(ParsePieceError::InvalidChar('X'))));
    /// ```
    InvalidChar(char),
}

impl fmt::Display for ParsePieceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsePieceError::InvalidLength(len) => {
                // Corrected expected length in message
                write!(f, "Invalid piece string length: {}, expected 1", len)
            }
            ParsePieceError::InvalidChar(char) => {
                write!(
                    f,
                    "Invalid FEN character for piece: '{}'", // Adjusted message for clarity
                    char
                )
            }
        }
    }
}

impl std::error::Error for ParsePieceError {}

/******************************************\
|==========================================|
|             Square Add Errors            |
|==========================================|
\******************************************/

/// Represents errors that can occur when performing arithmetic operations on a `Square`,
/// such as adding a direction vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SquareAddError {
    /// The resulting square after the addition would be outside the bounds of the chessboard (0-63).
    ///
    /// # Example
    /// ```
    /// # use chess::core::{Square, Direction};
    /// # use chess::core::errors::SquareAddError;
    /// # // Dummy Direction enum for example
    /// # #[derive(PartialEq, Eq, Clone, Copy, Debug)] enum Direction { NORTH, SOUTH, EAST, WEST }
    /// # // Dummy Square enum for example
    /// # #[derive(PartialEq, Eq, Clone, Copy, Debug)] enum Square { A1, A2, H8 }
    /// # impl Square { fn index(&self) -> i8 { *self as i8 } } // Dummy index
    /// # fn add_direction(sq: Square, dir: Direction) -> Result<Square, SquareAddError> {
    /// #     // Dummy implementation for example
    /// #     if sq == Square::H8 && dir == Direction::NORTH { return Err(SquareAddError::OutOfBounds); } // Example failure
    /// #     if sq == Square::A1 && dir == Direction::SOUTH { return Err(SquareAddError::OutOfBounds); } // Example failure
    /// #     Ok(Square::A2) // Dummy success case
    /// # }
    /// let square = Square::A1;
    /// let direction = Direction::SOUTH; // Moving south from A1 goes off the board
    /// let result = add_direction(square, direction);
    /// assert_eq!(result, Err(SquareAddError::OutOfBounds));
    /// ```
    OutOfBounds,
}

impl fmt::Display for SquareAddError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SquareAddError::OutOfBounds => {
                write!(f, "Square operation resulted in an out-of-bounds position")
            } // Slightly more general message
        }
    }
}

impl std::error::Error for SquareAddError {}

/******************************************\
|==========================================|
|            Square Parse Errors           |
|==========================================|
\******************************************/

/// Represents errors that can occur when attempting to parse a `File` from a string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseFileError {
    /// The input string did not have the expected length (usually 1).
    InvalidLength(usize),
    /// The character is not a valid file character ('a' through 'h').
    InvalidChar(char),
}

impl fmt::Display for ParseFileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseFileError::InvalidLength(l) => {
                write!(f, "Invalid length for file string: {}, expected 1", l)
            }
            ParseFileError::InvalidChar(c) => {
                write!(
                    f,
                    "Invalid character for file string: '{}', expected 'a'-'h'",
                    c
                )
            }
        }
    }
}

impl std::error::Error for ParseFileError {}

/// Represents errors that can occur when attempting to parse a `Rank` from a string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseRankError {
    /// The input string did not have the expected length (usually 1).
    InvalidLength(usize),
    /// The character is not a valid rank character ('1' through '8').
    InvalidChar(char),
}

impl fmt::Display for ParseRankError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseRankError::InvalidLength(l) => {
                write!(f, "Invalid length for rank string: {}, expected 1", l)
            }
            ParseRankError::InvalidChar(c) => {
                write!(
                    f,
                    "Invalid character for rank string: '{}', expected '1'-'8'",
                    c
                )
            }
        }
    }
}

impl std::error::Error for ParseRankError {}

/// Represents errors that can occur when attempting to parse a `Square`
/// from algebraic notation (e.g., "e4", "h8").
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseSquareError {
    /// The input string did not have the expected length of 2 characters.
    /// Contains the actual length received.
    ///
    /// # Example
    /// ```
    /// # use chess::core::errors::ParseSquareError; // Corrected path
    /// # use chess::core::Square; // Assuming Square is in core
    /// let result = "e4g".parse::<Square>(); // Input is too long
    /// assert!(matches!(result, Err(ParseSquareError::InvalidLength(3))));
    /// let result = "e".parse::<Square>(); // Input is too short
    /// assert!(matches!(result, Err(ParseSquareError::InvalidLength(1))));
    /// ```
    InvalidLength(usize),

    /// The first character of the input string was not a valid file character ('a' through 'h').
    /// Contains the invalid character received.
    ///
    /// # Example
    /// ```
    /// # use chess::core::errors::ParseSquareError; // Corrected path
    /// # use chess::core::Square; // Assuming Square is in core
    /// let result = "z4".parse::<Square>(); // 'z' is not a valid file
    /// assert!(matches!(result, Err(ParseSquareError::InvalidFileChar('z'))));
    /// ```
    InvalidFileChar(char),

    /// The second character of the input string was not a valid rank character ('1' through '8').
    /// Contains the invalid character received.
    ///
    /// # Example
    /// ```
    /// # use chess::core::errors::ParseSquareError; // Corrected path
    /// # use chess::core::Square; // Assuming Square is in core
    /// let result = "e9".parse::<Square>(); // '9' is not a valid rank
    /// assert!(matches!(result, Err(ParseSquareError::InvalidRankChar('9'))));
    /// ```
    InvalidRankChar(char),
}

impl fmt::Display for ParseSquareError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseSquareError::InvalidLength(len) => {
                write!(f, "Invalid square string length: {}, expected 2", len)
            }
            ParseSquareError::InvalidFileChar(c) => {
                write!(f, "Invalid file character: '{}', expected 'a'-'h'", c)
            }
            ParseSquareError::InvalidRankChar(c) => {
                write!(f, "Invalid rank character: '{}', expected '1'-'8'", c)
            }
        }
    }
}

impl std::error::Error for ParseSquareError {}

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
#[derive(Debug, PartialEq, Eq, Clone)] // Added Clone for consistency if needed elsewhere
pub enum FenParseError {
    /// The FEN string did not contain exactly 6 fields separated by whitespace.
    ///
    /// # Example
    /// ```
    /// # use chess::core::Board; // Assuming Board::from_fen exists
    /// # use chess::core::errors::FenParseError; // Corrected path
    /// let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0"; // Missing fullmove number
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidNumberOfFields)));
    /// ```
    InvalidNumberOfFields,

    /// An invalid character was encountered in the piece placement field (the first field).
    /// Valid characters are 'p', 'n', 'b', 'r', 'q', 'k' (case-insensitive), digits '1'-'8', and '/'.
    /// Contains the invalid character.
    ///
    /// # Example
    /// ```
    /// # use chess::core::Board;
    /// # use chess::core::errors::FenParseError; // Corrected path
    /// let fen = "rnbqkbnr/ppppxppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"; // 'x' is invalid
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidPiecePlacementChar('x'))));
    /// ```
    InvalidPiecePlacementChar(char),

    /// A rank description within the piece placement field was malformed.
    /// This usually means the pieces and empty square counts ('1'-'8') for a rank
    /// do not sum up to exactly 8 files. It can also indicate missing '/' separators,
    /// too many separators, or invalid skip digits ('0', '9').
    /// Contains a string describing the specific format error.
    ///
    /// # Example
    /// ```
    /// # use chess::core::Board;
    /// # use chess::core::errors::FenParseError; // Corrected path
    /// // Rank 2 specifies 9 files (pppppppp + 1 -> error on '1')
    /// let fen = "rnbqkbnr/pppppppp1/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_)))); // Check message if needed
    ///
    /// // Missing a rank separator (results in final rank check failure)
    /// let fen = "rnbqkbnr/pppppppp8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_)))); // Check message if needed
    /// ```
    InvalidRankFormat(String),

    /// The side-to-move field (the second field) contained an invalid character.
    /// Expected 'w' for White or 'b' for Black.
    /// Contains the invalid string found.
    ///
    /// # Example
    /// ```
    /// # use chess::core::Board;
    /// # use chess::core::errors::FenParseError; // Corrected path
    /// let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1"; // 'x' is invalid
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidSideToMove(s)) if s == "x"));
    /// ```
    InvalidSideToMove(String),

    /// The castling availability field (the third field) contained an invalid character.
    /// Valid characters are 'K', 'Q', 'k', 'q', or '-' if no castling is possible.
    /// For Chess960, valid characters are file letters 'A'-'H' and 'a'-'h', or '-'.
    /// Contains the first invalid character encountered.
    ///
    /// # Example
    /// ```
    /// # use chess::core::Board;
    /// # use chess::core::errors::FenParseError; // Corrected path
    /// let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQXkq - 0 1"; // 'X' is invalid
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidCastlingChar('X'))));
    /// ```
    InvalidCastlingChar(char),

    /// The en passant target square field (the fourth field) was not '-' and did not
    /// represent a valid square in algebraic notation (e.g., "e3", "f6") on the correct rank (3 or 6).
    /// Contains the invalid string found.
    ///
    /// # Example
    /// ```
    /// # use chess::core::Board;
    /// # use chess::core::errors::FenParseError; // Corrected path
    /// let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e9 0 1"; // "e9" is invalid square
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidEnPassantSquare(s)) if s == "e9"));
    ///
    /// let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e4 0 1"; // "e4" is invalid rank for EP
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidEnPassantSquare(s)) if s.contains("e4 is not a valid enpassant square"))));
    /// ```
    InvalidEnPassantSquare(String), // Contains the invalid string or a more descriptive error

    /// The halfmove clock field (the fifth field) could not be parsed as a non-negative integer (u8).
    /// This clock counts halfmoves since the last capture or pawn advance, used for the 50-move rule.
    /// Contains the invalid string found.
    ///
    /// # Example
    /// ```
    /// # use chess::core::Board;
    /// # use chess::core::errors::FenParseError; // Corrected path
    /// let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - fifty 1"; // "fifty" is invalid
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidHalfmoveClock(s)) if s == "fifty"));
    ///
    /// let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - -1 1"; // Negative is invalid
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidHalfmoveClock(s)) if s == "-1"));
    /// ```
    InvalidHalfmoveClock(String),

    /// The fullmove number field (the sixth field) could not be parsed as a positive integer (u16, >= 1).
    /// This number starts at 1 and increments after each Black move.
    /// Contains the invalid string found.
    ///
    /// # Example
    /// ```
    /// # use chess::core::Board;
    /// # use chess::core::errors::FenParseError; // Corrected path
    /// let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 zero"; // "zero" is invalid
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidFullmoveNumber(s)) if s == "zero"));
    ///
    /// let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"; // 0 is invalid
    /// let result = Board::from_fen(fen);
    /// assert!(matches!(result, Err(FenParseError::InvalidFullmoveNumber(s)) if s.contains("cannot be 0")));
    /// ```
    InvalidFullmoveNumber(String),
}

impl fmt::Display for FenParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FenParseError::InvalidNumberOfFields => {
                write!(f, "FEN string must have 6 fields separated by spaces")
            }
            FenParseError::InvalidPiecePlacementChar(c) => {
                write!(f, "Invalid character in FEN piece placement: '{}'", c)
            }
            FenParseError::InvalidRankFormat(reason) => write!(
                f,
                "Invalid rank format in FEN piece placement: {}", // Removed 'reason:' prefix as reason is descriptive
                reason
            ),
            FenParseError::InvalidSideToMove(s) => {
                write!(
                    f,
                    "Invalid side to move in FEN: '{}', expected 'w' or 'b'",
                    s
                )
            }
            FenParseError::InvalidCastlingChar(c) => {
                write!(f, "Invalid character in FEN castling availability: '{}'", c)
            }
            FenParseError::InvalidEnPassantSquare(s) => {
                write!(f, "Invalid en passant target square in FEN: '{}'", s)
            }
            FenParseError::InvalidHalfmoveClock(s) => {
                write!(f, "Invalid halfmove clock value in FEN: '{}'", s)
            }
            FenParseError::InvalidFullmoveNumber(s) => {
                write!(f, "Invalid fullmove number value in FEN: '{}'", s)
            }
        }
    }
}

// Implement the Error trait for FenParseError
impl std::error::Error for FenParseError {}
