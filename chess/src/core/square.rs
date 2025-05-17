use super::types::Colour;
use thiserror::Error;

/******************************************\
|==========================================|
|                 Squares                  |
|==========================================|
\******************************************/

/// # Square representation
/// 
/// - Represents the squares of a chess board 

#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Square {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
}

impl Square {
    /// Number of elements in the Square enum
    pub const NUM: usize = 64;
}

crate::impl_from_to_primitive!(Square);
crate::impl_enum_iter!(Square);

/******************************************\
|==========================================|
|                  Ranks                   |
|==========================================|
\******************************************/

/// # Ranks representation
/// 
/// - Represents the ranks of a chess board

#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum Rank {
    Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8,
}

impl Rank {
    /// Number of elements in the Rank enum
    pub const NUM: usize = 8;
}

crate::impl_from_to_primitive!(Rank);
crate::impl_enum_iter!(Rank);

/******************************************\
|==========================================|
|                  Files                   |
|==========================================|
\******************************************/

/// # Files representation
/// 
/// - Represents the files of a chess board

#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum File {
    FileA, FileB, FileC, FileD, FileE, FileF, FileG, FileH,
}

impl File {
    /// Number of elements in the File enum
    pub const NUM: usize = 8;
}

crate::impl_from_to_primitive!(File);
crate::impl_enum_iter!(File);

/******************************************\
|==========================================|
|              Implementation              |
|==========================================|
\******************************************/

impl Square {
    /// Returns the rank of a square
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::{Square, Rank};
    ///
    /// assert_eq!(Square::A1.rank(), Rank::Rank1);
    /// assert_eq!(Square::E4.rank(), Rank::Rank4);
    /// assert_eq!(Square::H8.rank(), Rank::Rank8);
    /// ```
    pub const fn rank(&self) -> Rank {
        let rank_index = (*self as u8) >> 3;
        unsafe { Rank::from_unchecked(rank_index) }
    }

    /// Returns the file of a square
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::{Square, File};
    ///
    /// assert_eq!(Square::A1.file(), File::FileA);
    /// assert_eq!(Square::E4.file(), File::FileE);
    /// assert_eq!(Square::H8.file(), File::FileH);
    /// ```
    pub const fn file(&self) -> File {
        let file_index = (*self as u8) & 0b111;
        unsafe { File::from_unchecked(file_index) }
    }

    /// Flips the rank of a square along the middle of the board, or switch perspectives between white and black
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::Square;
    ///
    /// assert_eq!(Square::A1.flip_rank(), Square::A8);
    /// assert_eq!(Square::E4.flip_rank(), Square::E5);
    /// assert_eq!(Square::H8.flip_rank(), Square::H1);
    /// ```
    pub const fn flip_rank(&self) -> Self {
        unsafe { Self::from_unchecked((*self as u8) ^ Square::A8 as u8) }
    }

    /// Returns the square relative to the perspectives of `col: Colour`
    ///
    /// For White, the square remains the same.
    /// For Black, the square's rank is flipped.
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::{Square, Colour};
    ///
    /// assert_eq!(Square::E2.relative(Colour::White), Square::E2);
    /// assert_eq!(Square::E2.relative(Colour::Black), Square::E7); // E2 from Black's perspective is E7
    ///
    /// assert_eq!(Square::D7.relative(Colour::White), Square::D7);
    /// assert_eq!(Square::D7.relative(Colour::Black), Square::D2); // D7 from Black's perspective is D2
    /// ```
    pub const fn relative(&self, col: Colour) -> Self {
        match col {
            Colour::White => *self,
            Colour::Black => self.flip_rank(),
        }
    }

    /// Returns the absolute distance in the ranks of two squares
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::Square;
    ///
    /// assert_eq!(Square::rank_dist(Square::E2, Square::E4), 2);
    /// assert_eq!(Square::rank_dist(Square::A1, Square::A8), 7);
    /// assert_eq!(Square::rank_dist(Square::H5, Square::H5), 0);
    /// assert_eq!(Square::rank_dist(Square::B7, Square::C2), 5); // (Rank7 - Rank2)
    /// ```
    pub const fn rank_dist(sq1: Square, sq2: Square) -> u8 {
        let v1 = sq1.rank() as u8;
        let v2 = sq2.rank() as u8;
        v1.abs_diff(v2)
    }

    /// Returns the absolute distance in the files of two squares
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::Square;
    ///
    /// assert_eq!(Square::file_dist(Square::A1, Square::D1), 3); // FileA to FileD
    /// assert_eq!(Square::file_dist(Square::H8, Square::A8), 7); // FileH to FileA
    /// assert_eq!(Square::file_dist(Square::E4, Square::E5), 0);
    /// assert_eq!(Square::file_dist(Square::B7, Square::G2), 5); // (FileB - FileG)
    /// ```
    pub const fn file_dist(sq1: Square, sq2: Square) -> u8 {
        let v1 = sq1.file() as u8;
        let v2 = sq2.file() as u8;
        v1.abs_diff(v2)
    }

    /// Combines a pair of file and rank to create a square
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::{Square, File, Rank};
    ///
    /// assert_eq!(Square::from_parts(File::FileA, Rank::Rank1), Square::A1);
    /// assert_eq!(Square::from_parts(File::FileE, Rank::Rank4), Square::E4);
    /// assert_eq!(Square::from_parts(File::FileH, Rank::Rank8), Square::H8);
    /// ```
    pub const fn from_parts(file: File, rank: Rank) -> Self {
        let index = ((rank as u8) << 3) + (file as u8);
        unsafe { Self::from_unchecked(index) }
    }
}

impl Rank {
    /// Flips rank along the middle of the board, or switch perspectives between white and black
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::Rank;
    ///
    /// assert_eq!(Rank::Rank1.flip(), Rank::Rank8);
    /// assert_eq!(Rank::Rank4.flip(), Rank::Rank5);
    /// ```
    pub const fn flip(&self) -> Self {
        unsafe { Self::from_unchecked(7 - (*self as u8)) }
    }

    /// Returns the rank relative to the perspectives of `col: Colour`
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::{Rank, Colour};
    ///
    /// assert_eq!(Rank::Rank2.relative(Colour::White), Rank::Rank2);
    /// assert_eq!(Rank::Rank2.relative(Colour::Black), Rank::Rank7);
    ///
    /// assert_eq!(Rank::Rank7.relative(Colour::White), Rank::Rank7);
    /// assert_eq!(Rank::Rank7.relative(Colour::Black), Rank::Rank2);
    /// ```
    pub const fn relative(&self, col: Colour) -> Self {
        match col {
            Colour::White => *self,
            Colour::Black => self.flip(),
        }
    }
}

/******************************************\
|==========================================|
|                 Display                  |
|==========================================|
\******************************************/

impl std::fmt::Display for File {
    /// Displays the file in the form of its chess board representation (FileA => 'a')
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", (b'a' + (*self as u8)) as char)
    }
}

impl std::fmt::Display for Rank {
    /// Displays the rank in the form of its chess board representation (Rank1 => '1')
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", (b'1' + (*self as u8)) as char)
    }
}

impl std::fmt::Display for Square {
    /// Displays the square in the form of its chess board representation (Square::A1 => 'a1')
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

/******************************************\
|==========================================|
|              Parsing Strings             |
|==========================================|
\******************************************/

impl std::str::FromStr for File {
    type Err = ParseFileError;

    /// Parses the file string into a file, with error checking
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::{File, ParseFileError};
    /// use std::str::FromStr;
    ///
    /// assert_eq!(File::from_str("a").unwrap(), File::FileA);
    /// assert_eq!("h".parse::<File>().unwrap(), File::FileH);
    /// assert!(matches!("x".parse::<File>(), Err(ParseFileError::InvalidChar('x'))));
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 1 {
            return Err(ParseFileError::InvalidLength(s.len()));
        }

        let file_char = s.chars().next().unwrap();
        match file_char {
            'a'..='h' => unsafe { Ok(File::from_unchecked((file_char as u8 - b'a') as u8)) },
            _ => Err(ParseFileError::InvalidChar(file_char)),
        }
    }
}

impl std::str::FromStr for Rank {
    type Err = ParseRankError;

    /// Parses the rank string into a rank, with error checking
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::{Rank, ParseRankError};
    /// use std::str::FromStr;
    ///
    /// assert_eq!(Rank::from_str("1").unwrap(), Rank::Rank1);
    /// assert_eq!("8".parse::<Rank>().unwrap(), Rank::Rank8);
    /// assert!(matches!("9".parse::<Rank>(), Err(ParseRankError::InvalidChar('9'))));
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 1 {
            return Err(ParseRankError::InvalidLength(s.len()));
        }

        let rank_char = s.chars().next().unwrap();
        match rank_char {
            '1'..='8' => unsafe { Ok(Rank::from_unchecked((rank_char as u8 - b'1') as u8)) },
            _ => Err(ParseRankError::InvalidChar(rank_char)),
        }
    }
}

impl std::str::FromStr for Square {
    type Err = ParseSquareError;

    /// Parses the square string into a square, with error checking
    ///
    /// ## Examples
    ///
    /// ```
    /// use chess::core::{Square, ParseSquareError};
    /// use std::str::FromStr;
    ///
    /// assert_eq!(Square::from_str("a1").unwrap(), Square::A1);
    /// assert_eq!("h8".parse::<Square>().unwrap(), Square::H8);
    /// assert!(matches!("e9".parse::<Square>(), Err(ParseSquareError::InvalidRankChar('9'))));
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 2 {
            return Err(ParseSquareError::InvalidLength(s.len()));
        }

        let mut chars = s.chars();
        let file_char = chars.next().unwrap();
        let rank_char = chars.next().unwrap();

        let file = file_char
            .to_string()
            .parse::<File>()
            .map_err(|_| ParseSquareError::InvalidFileChar(file_char))?;
        let rank = rank_char
            .to_string()
            .parse::<Rank>()
            .map_err(|_| ParseSquareError::InvalidRankChar(rank_char))?;

        Ok(Square::from_parts(file, rank))
    }
}

/******************************************\
|==========================================|
|            Square Parse Errors           |
|==========================================|
\******************************************/

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParseFileError {
    #[error("Invalid length for file string: {0}, expected 1")]
    InvalidLength(usize),
    #[error("Invalid character for file string: '{0}', expected 'a'-'h'")]
    InvalidChar(char),
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParseRankError {
    #[error("Invalid length for rank string: {0}, expected 1")]
    InvalidLength(usize),
    #[error("Invalid character for rank string: '{0}', expected '1'-'9'")]
    InvalidChar(char),
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParseSquareError {
    #[error("Invalid length for square string: {0}, expected 2")]
    InvalidLength(usize),
    #[error("Invalid character for file string: '{0}', expected 'a'-'h'")]
    InvalidFileChar(char),
    #[error("Invalid character for rank string: '{0}', expected '1'-'8'")]
    InvalidRankChar(char),
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
    fn test_square_from_tuple() {
        assert_eq!(Square::from_parts(File::FileA, Rank::Rank1), Square::A1);
        assert_eq!(Square::from_parts(File::FileE, Rank::Rank4), Square::E4);
        assert_eq!(Square::from_parts(File::FileH, Rank::Rank8), Square::H8);
    }

    #[test]
    fn test_square_into() {
        let square: Square = Square::from_parts(File::FileD, Rank::Rank5);
        assert_eq!(square, Square::D5);
    }

    #[test]
    fn test_file_and_rank() {
        let square = Square::C6;
        assert_eq!(square.file(), File::FileC);
        assert_eq!(square.rank(), Rank::Rank6);
    }

    #[test]
    fn test_flip_rank() {
        assert_eq!(Square::A1.flip_rank(), Square::A8);
        assert_eq!(Square::E4.flip_rank(), Square::E5);
        assert_eq!(Square::H8.flip_rank(), Square::H1);
    }

    #[test]
    fn test_square_conversions() {
        for file in 0..8 {
            for rank in 0..8 {
                let f = unsafe { File::from_unchecked(file) };
                let r = unsafe { Rank::from_unchecked(rank) };
                let square = Square::from_parts(f, r);
                assert_eq!(square.file(), f);
                assert_eq!(square.rank(), r);
            }
        }
    }

    #[test]
    fn test_square_from_str_valid() {
        assert_eq!("a1".parse::<Square>().unwrap(), Square::A1);
        assert_eq!("h8".parse::<Square>().unwrap(), Square::H8);
        assert_eq!("e4".parse::<Square>().unwrap(), Square::E4);
        assert_eq!("c7".parse::<Square>().unwrap(), Square::C7);
        assert_eq!("g2".parse::<Square>().unwrap(), Square::G2);
        assert_eq!("b5".parse::<Square>().unwrap(), Square::B5);
        assert_eq!("f3".parse::<Square>().unwrap(), Square::F3);
        assert_eq!("d6".parse::<Square>().unwrap(), Square::D6);
    }

    #[test]
    fn test_square_from_str_invalid() {
        assert!(matches!(
            "e".parse::<Square>(),
            Err(ParseSquareError::InvalidLength(1))
        ));
        assert!(matches!(
            "e4g".parse::<Square>(),
            Err(ParseSquareError::InvalidLength(3))
        ));
        assert!(matches!(
            "".parse::<Square>(),
            Err(ParseSquareError::InvalidLength(0))
        ));

        assert!(matches!(
            "z4".parse::<Square>(),
            Err(ParseSquareError::InvalidFileChar('z'))
        ));
        assert!(matches!(
            "i1".parse::<Square>(),
            Err(ParseSquareError::InvalidFileChar('i'))
        ));
        assert!(matches!(
            "@5".parse::<Square>(),
            Err(ParseSquareError::InvalidFileChar('@'))
        ));

        assert!(matches!(
            "A1".parse::<Square>(),
            Err(ParseSquareError::InvalidFileChar('A'))
        ));

        assert!(matches!(
            "a9".parse::<Square>(),
            Err(ParseSquareError::InvalidRankChar('9'))
        ));
        assert!(matches!(
            "h0".parse::<Square>(),
            Err(ParseSquareError::InvalidRankChar('0'))
        ));
        assert!(matches!(
            "eA".parse::<Square>(),
            Err(ParseSquareError::InvalidRankChar('A'))
        ));
        assert!(matches!(
            "f ".parse::<Square>(),
            Err(ParseSquareError::InvalidRankChar(' '))
        ));

        assert!(matches!(
            "z9".parse::<Square>(),
            Err(ParseSquareError::InvalidFileChar('z'))
        ));
        assert!(matches!(
            "1a".parse::<Square>(),
            Err(ParseSquareError::InvalidFileChar('1'))
        ));
    }
}
