use crate::core::Colour;
use macros::{EnumIter, FromPrimitive};
/******************************************\
|==========================================|
|                  Piece                   |
|==========================================|
\******************************************/

/// # Piece Representation  (Type: u8)
/// 
/// Represents chess pieces with both color and type information encoded.
/// Piece values are designed to allow easy extraction of type and color.
/// 
/// ```rust,no_run
/// WhitePawn = 1, WhiteKnight = 2, WhiteBishop = 3, WhiteRook = 4, WhiteQueen = 5, WhiteKing = 6,
/// BlackPawn = 9, BlackKnight = 10, BlackBishop = 11, BlackRook = 12, BlackQueen = 13, BlackKing = 14,
/// ```
/// 
/// ## Encoding Format
/// Each piece is stored in a single byte with the following bit layout:
/// 
/// | Bits   | Purpose                     | Values                |
/// |--------|-----------------------------|-----------------------|
/// | 0-2    | Piece type                  | 0=Pawn, 1=Knight, ... |
/// | 3      | Color                       | 0=White, 1=Black      |
/// 
/// This encoding results in these numeric values:
/// 
/// | Piece       | White | Black |
/// |-------------|-------|-------|
/// | Pawn        | 1     | 9     |
/// | Knight      | 2     | 10    |
/// | Bishop      | 3     | 11    |
/// | Rook        | 4     | 12    |
/// | Queen       | 5     | 13    |
/// | King        | 6     | 14    |
/// 
/// ## Features
/// - Implements `EnumIter` for iteration
/// - Implements `FromPrimitive` for numeric conversion
/// - Methods for extracting `piece_type()` and `piece_color()`
/// - Creation from color and type via `From<(Colour, PieceType)>`
/// - Length variable: `Piece::NUM = 12`
/// 
/// ## Usage Examples
/// ```rust,no_run
/// 
/// // Create piece from color and type
/// let white_queen = Piece::from((Colour::White, PieceType::Queen));
/// 
/// // Extract piece components
/// let piece_type = white_queen.piece_type();  // PieceType::Queen
/// let color = white_queen.piece_color();      // Colour::White
/// 
/// // Convert between numeric value and piece
/// let numeric_value = white_queen as u8;      // 5
/// let piece = Piece::from(numeric_value);     // Piece::WhiteQueen
/// ```
#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter, FromPrimitive)]
pub enum Piece {
    WhitePawn = 1, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing, BlackPawn = 9, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing,
}

/******************************************\
|==========================================|
|                Piece Type                |
|==========================================|
\******************************************/

/// # Piece Type Representation  (Type: u8)
/// 
/// Represents the types of chess pieces without color information.
/// 
/// ```rust,no_run
/// Pawn = 1, Knight = 2, Bishop = 3, Rook = 4, Queen = 5, King = 6,
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
/// // Creating a piece from color and type
/// let black_bishop = Piece::from((Colour::Black, PieceType::Bishop));
/// 
/// // Extracting type from a piece
/// let piece_type = black_bishop.piece_type();  // PieceType::Bishop
/// 
/// // Converting between numeric value and piece type
/// let numeric_value = piece_type as u8;       // 3
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
   Pawn = 1, Knight, Bishop, Rook, Queen, King,
}

/******************************************\
|==========================================|
|              Implementation              |
|==========================================|
\******************************************/

impl Piece {
    /// # Get Piece Type
    ///
    /// Extracts the piece type from a piece by masking out the color bit.
    ///
    /// Returns the corresponding `PieceType` enum value.
    pub fn piece_type(&self) -> PieceType {
        ((*self as u8) & 0b111).into()
    }

    /// # Get Piece Color
    ///
    /// Extracts the color from a piece by checking the color bit.
    ///
    /// Returns the corresponding `Colour` enum value.
    pub fn piece_color(&self) -> Colour {
        ((*self as u8) >> 3).into()
    }
}

/// # Create Piece from Color and Type
///
/// Allows creating a piece by combining a color and a piece type.
///
/// This encodes the color in bit 3 and the piece type in bits 0-2.
impl From<(Colour, PieceType)> for Piece {
    fn from((colour, piece_type): (Colour, PieceType)) -> Self {
        ((colour as u8) << 3 | piece_type as u8).into()
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
        assert_eq!(Piece::WhitePawn.piece_type(), PieceType::Pawn);
        assert_eq!(Piece::WhiteKnight.piece_type(), PieceType::Knight);
        assert_eq!(Piece::WhiteBishop.piece_type(), PieceType::Bishop);
        assert_eq!(Piece::WhiteRook.piece_type(), PieceType::Rook);
        assert_eq!(Piece::WhiteQueen.piece_type(), PieceType::Queen);
        assert_eq!(Piece::WhiteKing.piece_type(), PieceType::King);

        assert_eq!(Piece::BlackPawn.piece_type(), PieceType::Pawn);
        assert_eq!(Piece::BlackKnight.piece_type(), PieceType::Knight);
        assert_eq!(Piece::BlackBishop.piece_type(), PieceType::Bishop);
        assert_eq!(Piece::BlackRook.piece_type(), PieceType::Rook);
        assert_eq!(Piece::BlackQueen.piece_type(), PieceType::Queen);
        assert_eq!(Piece::BlackKing.piece_type(), PieceType::King);
    }

    #[test]
    fn test_piece_color_extraction() {
        // Test extracting color from pieces
        assert_eq!(Piece::WhitePawn.piece_color(), Colour::White);
        assert_eq!(Piece::WhiteKnight.piece_color(), Colour::White);
        assert_eq!(Piece::WhiteBishop.piece_color(), Colour::White);
        assert_eq!(Piece::WhiteRook.piece_color(), Colour::White);
        assert_eq!(Piece::WhiteQueen.piece_color(), Colour::White);
        assert_eq!(Piece::WhiteKing.piece_color(), Colour::White);

        assert_eq!(Piece::BlackPawn.piece_color(), Colour::Black);
        assert_eq!(Piece::BlackKnight.piece_color(), Colour::Black);
        assert_eq!(Piece::BlackBishop.piece_color(), Colour::Black);
        assert_eq!(Piece::BlackRook.piece_color(), Colour::Black);
        assert_eq!(Piece::BlackQueen.piece_color(), Colour::Black);
        assert_eq!(Piece::BlackKing.piece_color(), Colour::Black);
    }

    #[test]
    fn test_create_piece_from_color_and_type() {
        // Test creating pieces from color and type
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
        assert_eq!(Piece::from(1), Piece::WhitePawn);
        assert_eq!(Piece::from(2), Piece::WhiteKnight);
        assert_eq!(Piece::from(3), Piece::WhiteBishop);
        assert_eq!(Piece::from(4), Piece::WhiteRook);
        assert_eq!(Piece::from(5), Piece::WhiteQueen);
        assert_eq!(Piece::from(6), Piece::WhiteKing);

        assert_eq!(Piece::from(9), Piece::BlackPawn);
        assert_eq!(Piece::from(10), Piece::BlackKnight);
        assert_eq!(Piece::from(11), Piece::BlackBishop);
        assert_eq!(Piece::from(12), Piece::BlackRook);
        assert_eq!(Piece::from(13), Piece::BlackQueen);
        assert_eq!(Piece::from(14), Piece::BlackKing);
    }

    #[test]
    fn test_piece_type_from_numeric_value() {
        // Test piece type from numeric value
        assert_eq!(PieceType::from(1), PieceType::Pawn);
        assert_eq!(PieceType::from(2), PieceType::Knight);
        assert_eq!(PieceType::from(3), PieceType::Bishop);
        assert_eq!(PieceType::from(4), PieceType::Rook);
        assert_eq!(PieceType::from(5), PieceType::Queen);
        assert_eq!(PieceType::from(6), PieceType::King);
    }

    #[test]
    fn test_piece_conversion_roundtrip() {
        // Test roundtrip: piece -> (color, type) -> piece
        for piece in Piece::iter() {
            let color = piece.piece_color();
            let piece_type = piece.piece_type();
            let reconstructed = Piece::from((color, piece_type));
            assert_eq!(piece, reconstructed);
        }
    }
}
