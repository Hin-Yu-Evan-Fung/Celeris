//! # Module: `square`
//!
//! This module defines the core types for representing squares, ranks, and files on a chessboard.
//! It provides the `Square`, `Rank`, and `File` enums, along with methods for manipulating and
//! converting between these types.
//!
//! ## Overview
//!
//! This module is a fundamental part of the chess engine, providing the basic building blocks for
//! representing the board's geometry. It defines the 64 squares, the 8 ranks, and the 8 files,
//! along with methods for converting between them and performing common operations.
//!
//! ## Key Components
//!
//! - **`Square`**: An enum representing the 64 squares on a chessboard.
//!   - Each square is identified by its file (A-H) and rank (1-8).
//!   - Implements `From<(File, Rank)>` for creating squares from file and rank.
//!   - Provides methods to extract file and rank components (`file()`, `rank()`).
//!   - Supports flipping across ranks or files (`flip_rank()`, `flip_file()`).
//!   - Implements `FromStr` for parsing squares from algebraic notation (e.g., "e4").
//!   - Supports iteration with `Square::iter()`.
//!   - Length variable: `Square::NUM = 64`.
//! - **`Rank`**: An enum representing the 8 ranks (rows) on a chessboard.
//!   - Ranks are numbered from bottom to top, with Rank1 at the bottom and Rank8 at the top.
//!   - Implements `PartialOrd` and `Ord` for natural rank ordering.
//!   - Implements `From<u8>` for numeric conversion.
//!   - Supports iteration with `Rank::iter()`.
//!   - Length variable: `Rank::NUM = 8`.
//! - **`File`**: An enum representing the 8 files (columns) on a chessboard.
//!   - Files are lettered from left to right, with FileA at the left and FileH at the right.
//!   - Implements `PartialOrd` and `Ord` for natural file ordering.
//!   - Implements `From<u8>` for numeric conversion.
//!   - Supports iteration with `File::iter()`.
//!   

use macros::{EnumIter, FromPrimitive};

use super::errors::ParseSquareError;
use crate::utils::abs_diff;

/******************************************\
|==========================================|
|                 Squares                  |
|==========================================|
\******************************************/

/// # Chess Square Representation
/// 
/// Represents all 64 squares on a standard chess board using algebraic notation.
/// Each square is identified by its file (A-H) and rank (1-8).
/// 
/// ```rust,no_run
/// 
/// // Square indices
/// const A1: u8 = 0;
/// const B1: u8 = 8;
/// const C1: u8 = 16;
/// // etc.
/// const H8: u8 = 63;
/// ```
/// 
/// ## Board Layout
/// ```
/// 8 | A8 B8 C8 D8 E8 F8 G8 H8
/// 7 | A7 B7 C7 D7 E7 F7 G7 H7
/// 6 | A6 B6 C6 D6 E6 F6 G6 H6
/// 5 | A5 B5 C5 D5 E5 F5 G5 H5
/// 4 | A4 B4 C4 D4 E4 F4 G4 H4
/// 3 | A3 B3 C3 D3 E3 F3 G3 H3
/// 2 | A2 B2 C2 D2 E2 F2 G2 H2
/// 1 | A1 B1 C1 D1 E1 F1 G1 H1
///   -------------------------
///     A  B  C  D  E  F  G  H
/// ```
/// 
/// ## Features
/// - Provides all 64 chess squares as enum variants
/// - Implements `From<u8>` for numeric conversion
/// - Supports iteration with `Square::iter()`
/// - Length variable: `Square::NUM = 64`
/// 
/// ## Implementation
/// Squares can be created from file and rank components using the `From` trait:
/// 
/// ```rust,no_run
/// use sophos::core::{Square, File, Rank};
/// 
/// let e4 = Square::from((File::FileE, Rank::Rank4));
/// // Or using the more concise into() method:
/// let e4: Square = (File::FileE, Rank::Rank4).into();
/// ```
/// 
/// You can extract file and rank components from a square:
/// 
/// ```rust,no_run
/// use sophos::core::{Square, File, Rank};
/// 
/// let square = Square::E4;
/// let file = square.file();  // File::FileE
/// let rank = square.rank();  // Rank::Rank4
/// ```
/// 
/// Squares support movement operations with the Direction type:
/// 
/// ```rust,no_run
/// 
/// let e4 = Square::E4;
/// let e5 = e4 + Direction::N;  // Some(Square::E5)
/// 
/// // Moves that would go off the board return None
/// let h1 = Square::H1;
/// let off_board = h1 + Direction::E;  // None
/// 
/// // In-place movement with bounds checking
/// let mut square = Square::E4;
/// square += Direction::N;  // square is now E5
/// square += Direction::E;  // square is now F5
/// square += Direction::S;  // square is now F4
/// 
/// // Attempting to move off the board doesn't change the square
/// let mut edge = Square::H1;
/// edge += Direction::E;  // still H1, doesn't move off the board
/// ```
/// 
/// Squares can also be flipped across ranks or files:
/// 
/// ```rust,no_run
/// 
/// let a1 = Square::A1;
/// let a8 = a1.flip_rank();  // Square::A8
/// let h1 = a1.flip_file();  // Square::H1
/// ```
#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter, FromPrimitive)]
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
    pub const NUM: usize = 64;
}

/******************************************\
|==========================================|
|                  Ranks                   |
|==========================================|
\******************************************/

/// # Rank Representation
/// 
/// Represents the 8 ranks (rows) on a chess board, from Rank1 to Rank8.
/// Ranks are numbered from bottom to top, with Rank1 at the bottom and Rank8 at the top.
/// 
/// ```rust,no_run
/// 
/// // Rank values
/// const RANK1: u8 = 0;
/// const RANK2: u8 = 1;
/// // ... other ranks ...
/// const RANK8: u8 = 7;
/// ```
/// 
/// ## Features
/// - Implements `PartialOrd` and `Ord` for natural rank ordering
/// - Implements `From<u8>` for numeric conversion
/// - Supports iteration with `Rank::iter()`
/// - Length variable: `Rank::NUM = 8`
/// 
/// In chess notation, ranks are represented by numbers 1-8.
#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, EnumIter, FromPrimitive)]
pub enum Rank {
    Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8,
}

impl Rank {
    pub const NUM: usize = 8;
}

/******************************************\
|==========================================|
|                  Files                   |
|==========================================|
\******************************************/

/// # File Representation
/// 
/// Represents the 8 files (columns) on a chess board, from FileA to FileH.
/// Files are lettered from left to right, with FileA at the left and FileH at the right.
/// 
/// ```rust,no_run
/// 
/// // File values
/// const FILE_A: u8 = 0;
/// const FILE_B: u8 = 1;
/// // ... other files ...
/// const FILE_H: u8 = 7;
/// ```
/// 
/// ## Features
/// - Implements `PartialOrd` and `Ord` for natural file ordering
/// - Implements `From<u8>` for numeric conversion
/// - Supports iteration with `File::iter()`
/// - Length variable: `File::NUM = 8`
/// 
/// In chess notation, files are represented by letters A-H.
#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, EnumIter, FromPrimitive)]
pub enum File {
    FileA, FileB, FileC, FileD, FileE, FileF, FileG, FileH,
}

impl File {
    pub const NUM: usize = 8;
}

/******************************************\
|==========================================|
|              Implementation              |
|==========================================|
\******************************************/

impl Square {
    /// Returns the rank of this square
    pub fn rank(&self) -> Rank {
        let rank_index = (*self as u8) >> 3;
        unsafe { Rank::from(rank_index) }
    }

    /// Returns the file of this square
    pub fn file(&self) -> File {
        let file_index = (*self as u8) & 0b111;
        unsafe { File::from(file_index) }
    }

    /// Flips the rank of this square
    pub fn flip_rank(&self) -> Self {
        unsafe { Self::from((*self as u8) ^ Square::A8 as u8) }
    }

    /// Flips the file of this square
    pub fn flip_file(&self) -> Self {
        unsafe { Self::from((*self as u8) ^ Square::H1 as u8) }
    }

    /// # Calculate Rank Distance
    ///
    /// Calculates the absolute distance between the ranks of two squares.
    ///
    /// # Arguments
    /// - `sq1`: The first square
    /// - `sq2`: The second square
    ///
    /// # Returns
    /// - The absolute difference between the ranks (0-7)
    ///
    /// # Examples
    /// ```rust,no_run
    /// use sophos::{Square};
    ///
    /// let a1 = Square::A1; // Rank 1
    /// let a8 = Square::A8; // Rank 8
    /// let rank_distance = Square::rank_dist(a1, a8); // 7
    /// ```
    pub fn rank_dist(sq1: Square, sq2: Square) -> u8 {
        let v1 = sq1.rank() as u8;
        let v2 = sq2.rank() as u8;
        abs_diff(v1, v2)
    }

    /// # Calculate File Distance
    ///
    /// Calculates the absolute distance between the files of two squares.
    ///
    /// # Arguments
    /// - `sq1`: The first square
    /// - `sq2`: The second square
    ///
    /// # Returns
    /// - The absolute difference between the files (0-7)
    ///
    /// # Examples
    /// ```rust,no_run
    /// use sophos::{Square};
    ///
    /// let a1 = Square::A1; // File A
    /// let h1 = Square::H1; // File H
    /// let file_distance = Square::file_dist(a1, h1); // 7
    /// ```
    pub fn file_dist(sq1: Square, sq2: Square) -> u8 {
        let v1 = sq1.file() as u8;
        let v2 = sq2.file() as u8;
        abs_diff(v1, v2)
    }

    /// ### Converts Square from File and Rank
    ///
    /// Allows creating a square by combining a file and a rank
    ///
    /// This encodes the rank in bit 4-6 and the piece type in bits 1-3.
    pub fn from_parts(file: File, rank: Rank) -> Self {
        let index = ((rank as u8) << 3) + (file as u8);
        unsafe { Self::from(index) }
    }
}

/******************************************\
|==========================================|
|                 Display                  |
|==========================================|
\******************************************/

/// Display function for squares
impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

/******************************************\
|==========================================|
|              Parsing Strings             |
|==========================================|
\******************************************/

/// Parses a square from algebraic notation (e.g., "e4").
impl std::str::FromStr for Square {
    type Err = ParseSquareError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 2 {
            return Err(ParseSquareError::InvalidLength(s.len()));
        }

        let mut chars = s.chars();
        let file_char = chars.next().unwrap(); // Safe due to length check
        let rank_char = chars.next().unwrap(); // Safe due to length check

        let file = match file_char {
            'a'..='h' => unsafe { File::from((file_char as u8 - b'a') as u8) },
            _ => return Err(ParseSquareError::InvalidFileChar(file_char)),
        };

        let rank = match rank_char {
            '1'..='8' => unsafe { Rank::from((rank_char as u8 - b'1') as u8) },
            _ => return Err(ParseSquareError::InvalidRankChar(rank_char)),
        };

        Ok(Square::from_parts(file, rank))
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
    fn test_flip_file() {
        assert_eq!(Square::A1.flip_file(), Square::H1);
        assert_eq!(Square::E4.flip_file(), Square::D4);
        assert_eq!(Square::D7.flip_file(), Square::E7);
    }

    #[test]
    fn test_square_conversions() {
        // Test all squares by creating them from file and rank and then extracting file and rank back
        for file in 0..8 {
            for rank in 0..8 {
                let f = unsafe { File::from(file) };
                let r = unsafe { Rank::from(rank) };
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
        // Invalid Length
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

        // Invalid File
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
        // Check case sensitivity (only lowercase files are valid)
        assert!(matches!(
            "A1".parse::<Square>(),
            Err(ParseSquareError::InvalidFileChar('A'))
        ));

        // Invalid Rank
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

        // Both Invalid (File error usually detected first)
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
