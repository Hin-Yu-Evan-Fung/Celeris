use super::errors::ParsePieceError;
use crate::core::Colour;
use macros::{EnumIter, FromPrimitive};

/******************************************\
|==========================================|
|                  Piece                   |
|==========================================|
\******************************************/
/// # Piece Representation
/// 
/// Represents chess pieces with both colour and type information encoded.
/// Piece values are designed to allow easy extraction of type and colour.
/// 
/// ```rust,no_run
/// WhitePawn = 0, WhiteKnight = 1, WhiteBishop = 2, WhiteRook = 3, WhiteQueen = 4, WhiteKing = 5,
/// BlackPawn = 8, BlackKnight = 9, BlackBishop = 10, BlackRook = 11, BlackQueen = 12, BlackKing = 13,
/// ```
/// 
/// ## Encoding Format
/// Each piece is stored in a single byte with the following bit layout:
/// 
/// | Bits   | Purpose                     | Values                |
/// |--------|-----------------------------|-----------------------|
/// | 0-2    | Piece type                  | 0=Pawn, 1=Knight, ... |
/// | 3      | Colour                       | 0=White, 1=Black      |
/// 
/// This encoding results in these numeric values:
/// 
/// | Piece       | White | Black |
/// |-------------|-------|-------|
/// | Pawn        | 0     | 8     |
/// | Knight      | 1     | 9     |
/// | Bishop      | 2     | 10    |
/// | Rook        | 3     | 11    |
/// | Queen       | 4     | 12    |
/// | King        | 5     | 13    |
/// 
/// ## Features
/// - Implements `EnumIter` for iteration
/// - Implements `FromPrimitive` for numeric conversion
/// - Methods for extracting `piece_type()` and `piece_colour()`
/// - Creation from colour and type via `From<(Colour, PieceType)>`
/// - Length variable: `Piece::NUM = 12`
/// 
/// ## Usage Examples
/// ```rust,no_run
/// 
/// // Create piece from colour and type
/// let white_queen = Piece::from((Colour::White, PieceType::Queen));
/// 
/// // Extract piece components
/// let piece_type = white_queen.piece_type();  // PieceType::Queen
/// let colour = white_queen.piece_colour();      // Colour::White
/// 
/// // Convert between numeric value and piece
/// let numeric_value = white_queen as u8;      // 4
/// let piece = Piece::from(numeric_value);     // Piece::WhiteQueen
/// ```
#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter, FromPrimitive)]
pub enum Piece {
    WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing, BlackPawn = 8, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing,
}

/******************************************\
|==========================================|
|                Piece Type                |
|==========================================|
\******************************************/

/// # Piece Type Representation  (Type: u8)
/// 
/// Represents the types of chess pieces without colour information.
/// 
/// ```rust,no_run
/// Pawn = 0, Knight = 1, Bishop = 2, Rook = 3, Queen = 4, King = 5,
/// ```
/// 
/// ## Features
/// - Implements `FromPrimitive` for numeric conversion
/// - Supports iteration with `PieceType::iter()`
/// - Length variable: `PieceType::NUM = 6`
/// - Pieces are ordered by their approximate value (Pawn → Knight → Bishop → Rook → Queen → King)
/// - Used in combination with `Colour` to form complete `Piece` representations
/// 
/// ## Usage Examples
/// ```rust,no_run
/// 
/// // Creating a piece from colour and type
/// let black_bishop = Piece::from((Colour::Black, PieceType::Bishop));
/// 
/// // Extracting type from a piece
/// let piece_type = black_bishop.piece_type();  // PieceType::Bishop
/// 
/// // Converting between numeric value and piece type
/// let numeric_value = piece_type as u8;       // 2
/// let piece_type = PieceType::from(numeric_value); // PieceType::Bishop
/// 
/// // Iterate over all piece types
/// for pt in PieceType::iter() {
///     println!("{:?}", pt);
/// }
/// ```
#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter, FromPrimitive)]
pub enum PieceType {
   Pawn, Knight, Bishop, Rook, Queen, King,
}

/******************************************\
|==========================================|
|              Implementation              |
|==========================================|
\******************************************/

impl Piece {
    /// # Get Piece Type
    ///
    /// Extracts the piece type from a piece by masking out the colour bit.
    ///
    /// Returns the corresponding `PieceType` enum value.
    pub fn piecetype(&self) -> PieceType {
        ((*self as u8) & 0b111).into()
    }

    /// # Get Piece Colour
    ///
    /// Extracts the colour from a piece by checking the colour bit.
    ///
    /// Returns the corresponding `Colour` enum value.
    pub fn colour(&self) -> Colour {
        ((*self as u8) >> 3).into()
    }
}

/// # Create Piece from Colour and Type
///
/// Allows creating a piece by combining a colour and a piece type.
///
/// This encodes the colour in bit 3 and the piece type in bits 0-2.
impl From<(Colour, PieceType)> for Piece {
    fn from((colour, piece_type): (Colour, PieceType)) -> Self {
        ((colour as u8) << 3 | piece_type as u8).into()
    }
}

/******************************************\
|==========================================|
|                 Display                  |
|==========================================|
\******************************************/

const PIECE_STR: &str = "PNBRQK  pnbrqk";

/// Display function for piece types
impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let piece_char = PIECE_STR.chars().nth(*self as usize).unwrap();
        write!(f, "{}", piece_char)
    }
}

/******************************************\
|==========================================|
|                Parse Piece               |
|==========================================|
\******************************************/

/// Parses a piece from its standard FEN character (e.g., 'P', 'n', 'K').
impl std::str::FromStr for Piece {
    type Err = ParsePieceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 1 {
            return Err(ParsePieceError::InvalidLength(s.len()));
        }

        let piece_char = s.chars().next().ok_or(ParsePieceError::InvalidLength(0))?;
        let index = PIECE_STR
            .chars()
            .position(|c| c == piece_char && c != ' ')
            .ok_or(ParsePieceError::InvalidChar(piece_char))? as u8;

        Piece::try_from(index).map_err(|_| ParsePieceError::InvalidChar(piece_char))
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

    #[test]
    fn test_piece_type_extraction() {
        // Test extracting piece type from pieces
        assert_eq!(Piece::WhitePawn.piecetype(), PieceType::Pawn);
        assert_eq!(Piece::WhiteKnight.piecetype(), PieceType::Knight);
        assert_eq!(Piece::WhiteBishop.piecetype(), PieceType::Bishop);
        assert_eq!(Piece::WhiteRook.piecetype(), PieceType::Rook);
        assert_eq!(Piece::WhiteQueen.piecetype(), PieceType::Queen);
        assert_eq!(Piece::WhiteKing.piecetype(), PieceType::King);

        assert_eq!(Piece::BlackPawn.piecetype(), PieceType::Pawn);
        assert_eq!(Piece::BlackKnight.piecetype(), PieceType::Knight);
        assert_eq!(Piece::BlackBishop.piecetype(), PieceType::Bishop);
        assert_eq!(Piece::BlackRook.piecetype(), PieceType::Rook);
        assert_eq!(Piece::BlackQueen.piecetype(), PieceType::Queen);
        assert_eq!(Piece::BlackKing.piecetype(), PieceType::King);
    }

    #[test]
    fn test_piece_colour_extraction() {
        // Test extracting colour from pieces
        assert_eq!(Piece::WhitePawn.colour(), Colour::White);
        assert_eq!(Piece::WhiteKnight.colour(), Colour::White);
        assert_eq!(Piece::WhiteBishop.colour(), Colour::White);
        assert_eq!(Piece::WhiteRook.colour(), Colour::White);
        assert_eq!(Piece::WhiteQueen.colour(), Colour::White);
        assert_eq!(Piece::WhiteKing.colour(), Colour::White);

        assert_eq!(Piece::BlackPawn.colour(), Colour::Black);
        assert_eq!(Piece::BlackKnight.colour(), Colour::Black);
        assert_eq!(Piece::BlackBishop.colour(), Colour::Black);
        assert_eq!(Piece::BlackRook.colour(), Colour::Black);
        assert_eq!(Piece::BlackQueen.colour(), Colour::Black);
        assert_eq!(Piece::BlackKing.colour(), Colour::Black);
    }

    #[test]
    fn test_create_piece_from_colour_and_type() {
        // Test creating pieces from colour and type
        assert_eq!(
            Piece::from((Colour::White, PieceType::Pawn)),
            Piece::WhitePawn
        );
        assert_eq!(
            Piece::from((Colour::White, PieceType::Knight)),
            Piece::WhiteKnight
        );
        assert_eq!(
            Piece::from((Colour::White, PieceType::Bishop)),
            Piece::WhiteBishop
        );
        assert_eq!(
            Piece::from((Colour::White, PieceType::Rook)),
            Piece::WhiteRook
        );
        assert_eq!(
            Piece::from((Colour::White, PieceType::Queen)),
            Piece::WhiteQueen
        );
        assert_eq!(
            Piece::from((Colour::White, PieceType::King)),
            Piece::WhiteKing
        );

        assert_eq!(
            Piece::from((Colour::Black, PieceType::Pawn)),
            Piece::BlackPawn
        );
        assert_eq!(
            Piece::from((Colour::Black, PieceType::Knight)),
            Piece::BlackKnight
        );
        assert_eq!(
            Piece::from((Colour::Black, PieceType::Bishop)),
            Piece::BlackBishop
        );
        assert_eq!(
            Piece::from((Colour::Black, PieceType::Rook)),
            Piece::BlackRook
        );
        assert_eq!(
            Piece::from((Colour::Black, PieceType::Queen)),
            Piece::BlackQueen
        );
        assert_eq!(
            Piece::from((Colour::Black, PieceType::King)),
            Piece::BlackKing
        );
    }

    #[test]
    fn test_piece_from_numeric_value() {
        // Test piece from numeric value
        assert_eq!(Piece::from(0), Piece::WhitePawn);
        assert_eq!(Piece::from(1), Piece::WhiteKnight);
        assert_eq!(Piece::from(2), Piece::WhiteBishop);
        assert_eq!(Piece::from(3), Piece::WhiteRook);
        assert_eq!(Piece::from(4), Piece::WhiteQueen);
        assert_eq!(Piece::from(5), Piece::WhiteKing);

        assert_eq!(Piece::from(8), Piece::BlackPawn);
        assert_eq!(Piece::from(9), Piece::BlackKnight);
        assert_eq!(Piece::from(10), Piece::BlackBishop);
        assert_eq!(Piece::from(11), Piece::BlackRook);
        assert_eq!(Piece::from(12), Piece::BlackQueen);
        assert_eq!(Piece::from(13), Piece::BlackKing);
    }

    #[test]
    fn test_piece_type_from_numeric_value() {
        // Test piece type from numeric value
        assert_eq!(PieceType::from(0), PieceType::Pawn);
        assert_eq!(PieceType::from(1), PieceType::Knight);
        assert_eq!(PieceType::from(2), PieceType::Bishop);
        assert_eq!(PieceType::from(3), PieceType::Rook);
        assert_eq!(PieceType::from(4), PieceType::Queen);
        assert_eq!(PieceType::from(5), PieceType::King);
    }

    #[test]
    fn test_piece_conversion_roundtrip() {
        // Test roundtrip: piece -> (colour, type) -> piece
        for piece in Piece::iter() {
            let colour = piece.colour();
            let piece_type = piece.piecetype();
            let reconstructed = Piece::from((colour, piece_type));
            assert_eq!(piece, reconstructed);
        }
    }

    #[test]
    fn test_piece_from_str_valid() {
        assert_eq!("P".parse::<Piece>().unwrap(), Piece::WhitePawn);
        assert_eq!("N".parse::<Piece>().unwrap(), Piece::WhiteKnight);
        assert_eq!("B".parse::<Piece>().unwrap(), Piece::WhiteBishop);
        assert_eq!("R".parse::<Piece>().unwrap(), Piece::WhiteRook);
        assert_eq!("Q".parse::<Piece>().unwrap(), Piece::WhiteQueen);
        assert_eq!("K".parse::<Piece>().unwrap(), Piece::WhiteKing);
        assert_eq!("p".parse::<Piece>().unwrap(), Piece::BlackPawn);
        assert_eq!("n".parse::<Piece>().unwrap(), Piece::BlackKnight);
        assert_eq!("b".parse::<Piece>().unwrap(), Piece::BlackBishop);
        assert_eq!("r".parse::<Piece>().unwrap(), Piece::BlackRook);
        assert_eq!("q".parse::<Piece>().unwrap(), Piece::BlackQueen);
        assert_eq!("k".parse::<Piece>().unwrap(), Piece::BlackKing);
    }

    #[test]
    fn test_piece_from_str_invalid() {
        // Invalid Length
        assert!(matches!(
            "".parse::<Piece>(),
            Err(ParsePieceError::InvalidLength(0))
        ));
        assert!(matches!(
            "Pn".parse::<Piece>(),
            Err(ParsePieceError::InvalidLength(2))
        ));
        assert!(matches!(
            "pp".parse::<Piece>(),
            Err(ParsePieceError::InvalidLength(2))
        ));

        // Invalid Character
        assert!(matches!(
            "X".parse::<Piece>(),
            Err(ParsePieceError::InvalidChar('X'))
        ));
        assert!(matches!(
            " ".parse::<Piece>(),
            Err(ParsePieceError::InvalidChar(' '))
        ));
        assert!(matches!(
            "1".parse::<Piece>(),
            Err(ParsePieceError::InvalidChar('1'))
        ));
        assert!(matches!(
            "o".parse::<Piece>(),
            Err(ParsePieceError::InvalidChar('o'))
        )); // 'o' is not 'Q' or 'q'
        assert!(matches!(
            "O".parse::<Piece>(),
            Err(ParsePieceError::InvalidChar('O'))
        )); // 'O' is not 'Q' or 'q'
        assert!(matches!(
            "a".parse::<Piece>(),
            Err(ParsePieceError::InvalidChar('a'))
        ));
    }
}
