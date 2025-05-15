use std::fmt;

use crate::core::Colour;

/******************************************\
|==========================================|
|                  Piece                   |
|==========================================|
\******************************************/

/// # Piece representation
/// 
/// - Represents the different chess pieces 

#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Piece {
    WhitePawn, BlackPawn, WhiteKnight, BlackKnight, WhiteBishop, BlackBishop, WhiteRook, BlackRook, WhiteQueen, BlackQueen, WhiteKing, BlackKing,
}

impl Piece {
    pub const NUM: usize = 12;
}

crate::impl_from_to_primitive!(Piece);
crate::impl_enum_iter!(Piece);

/******************************************\
|==========================================|
|                Piece Type                |
|==========================================|
\******************************************/

#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceType {
   Pawn, Knight, Bishop, Rook, Queen, King,
}

impl PieceType {
    pub const NUM: usize = 6;
}

crate::impl_from_to_primitive!(PieceType);
crate::impl_enum_iter!(PieceType);

/******************************************\
|==========================================|
|              Implementation              |
|==========================================|
\******************************************/

impl Piece {
    pub const fn pt(self) -> PieceType {
        // PieceType::from_unchecked((*self as u8) & 0b111)
        PieceType::from_unchecked(self as u8 >> 1)
    }

    pub const fn colour(self) -> Colour {
        Colour::from_unchecked(self as u8 & 1)
    }

    pub const fn from_parts(colour: Colour, piece_type: PieceType) -> Self {
        Piece::from_unchecked(colour as u8 | (piece_type as u8) << 1)
    }
}

/******************************************\
|==========================================|
|                 Display                  |
|==========================================|
\******************************************/

const PIECE_STR: &str = "PpNnBbRrQqKk";

impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let piece_char = PIECE_STR.chars().nth(self.index()).unwrap();
        write!(f, "{}", piece_char)
    }
}

impl std::fmt::Display for PieceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let piece_char = PIECE_STR
            .chars()
            .nth(self.index() << 1)
            .unwrap()
            .to_ascii_lowercase();
        write!(f, "{}", piece_char)
    }
}

/******************************************\
|==========================================|
|                Parse Piece               |
|==========================================|
\******************************************/

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

        Ok(Piece::from_unchecked(index))
    }
}

/******************************************\
|==========================================|
|            Piece Parse Error             |
|==========================================|
\******************************************/

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsePieceError {
    InvalidLength(usize),
    InvalidChar(char),
}

impl fmt::Display for ParsePieceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsePieceError::InvalidLength(len) => {
                write!(f, "Invalid piece string length: {}, expected 1", len)
            }
            ParsePieceError::InvalidChar(char) => {
                write!(f, "Invalid FEN character for piece: '{}'", char)
            }
        }
    }
}

impl std::error::Error for ParsePieceError {}

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
        assert_eq!(Piece::WhitePawn.pt(), PieceType::Pawn);
        assert_eq!(Piece::WhiteKnight.pt(), PieceType::Knight);
        assert_eq!(Piece::WhiteBishop.pt(), PieceType::Bishop);
        assert_eq!(Piece::WhiteRook.pt(), PieceType::Rook);
        assert_eq!(Piece::WhiteQueen.pt(), PieceType::Queen);
        assert_eq!(Piece::WhiteKing.pt(), PieceType::King);

        assert_eq!(Piece::BlackPawn.pt(), PieceType::Pawn);
        assert_eq!(Piece::BlackKnight.pt(), PieceType::Knight);
        assert_eq!(Piece::BlackBishop.pt(), PieceType::Bishop);
        assert_eq!(Piece::BlackRook.pt(), PieceType::Rook);
        assert_eq!(Piece::BlackQueen.pt(), PieceType::Queen);
        assert_eq!(Piece::BlackKing.pt(), PieceType::King);
    }

    #[test]
    fn test_piece_colour_extraction() {
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
        assert_eq!(
            Piece::from_parts(Colour::White, PieceType::Pawn),
            Piece::WhitePawn
        );
        assert_eq!(
            Piece::from_parts(Colour::White, PieceType::Knight),
            Piece::WhiteKnight
        );
        assert_eq!(
            Piece::from_parts(Colour::White, PieceType::Bishop),
            Piece::WhiteBishop
        );
        assert_eq!(
            Piece::from_parts(Colour::White, PieceType::Rook),
            Piece::WhiteRook
        );
        assert_eq!(
            Piece::from_parts(Colour::White, PieceType::Queen),
            Piece::WhiteQueen
        );
        assert_eq!(
            Piece::from_parts(Colour::White, PieceType::King),
            Piece::WhiteKing
        );

        assert_eq!(
            Piece::from_parts(Colour::Black, PieceType::Pawn),
            Piece::BlackPawn
        );
        assert_eq!(
            Piece::from_parts(Colour::Black, PieceType::Knight),
            Piece::BlackKnight
        );
        assert_eq!(
            Piece::from_parts(Colour::Black, PieceType::Bishop),
            Piece::BlackBishop
        );
        assert_eq!(
            Piece::from_parts(Colour::Black, PieceType::Rook),
            Piece::BlackRook
        );
        assert_eq!(
            Piece::from_parts(Colour::Black, PieceType::Queen),
            Piece::BlackQueen
        );
        assert_eq!(
            Piece::from_parts(Colour::Black, PieceType::King),
            Piece::BlackKing
        );
    }

    #[test]
    fn test_piece_type_from_numeric_value() {
        assert_eq!(PieceType::from_unchecked(0), PieceType::Pawn);
        assert_eq!(PieceType::from_unchecked(1), PieceType::Knight);
        assert_eq!(PieceType::from_unchecked(2), PieceType::Bishop);
        assert_eq!(PieceType::from_unchecked(3), PieceType::Rook);
        assert_eq!(PieceType::from_unchecked(4), PieceType::Queen);
        assert_eq!(PieceType::from_unchecked(5), PieceType::King);
    }

    #[test]
    fn test_piece_conversion_roundtrip() {
        // for piece in Piece::iter() {
        //     let colour = piece.colour();
        //     let piece_type = piece.pt();
        //     let reconstructed = Piece::from_parts(colour, piece_type);
        //     assert_eq!(piece, reconstructed);
        // }
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
        ));
        assert!(matches!(
            "O".parse::<Piece>(),
            Err(ParsePieceError::InvalidChar('O'))
        ));
        assert!(matches!(
            "a".parse::<Piece>(),
            Err(ParsePieceError::InvalidChar('a'))
        ));
    }
}
