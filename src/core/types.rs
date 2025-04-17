//! # Module: `types`
//!
//! This module defines fundamental types used throughout the chess engine, including:
//!
//! - **`Colour`**: Represents the two colors in chess (White and Black).
//! - **`Direction`**: Represents movement directions on the chessboard.
//! - **`Castling`**: Represents castling rights for both players.
//!
//! ## Overview
//!
//! This module provides the basic building blocks for representing the state of a chess game.
//! It defines the colors, directions, and castling rights that are essential for move generation,
//! board evaluation, and game logic.
//!
//! ## Key Components
//!
//! - **`Colour`**: An enum with two variants, `White` and `Black`, representing the two players.
//!   - Implements `From<u8>` for numeric conversion.
//!   - Supports iteration with `Colour::iter()`.
//!   - Length variable: `Colour::NUM = 2`.
//! - **`Direction`**: An enum representing movement directions on the chessboard.
//!   - Provides all common chess movement directions (N, S, E, W, NE, NW, SE, SW, etc.).
//!   - Implements `From<i8>` for numeric conversion.
//!   - Semantic directional naming (N = North, S = South, etc.).
//!   - Can be applied to squares using the `+` operator, returning an `Option<Square>`.
//!   - Supports conversion between squares to find the direction using `TryFrom`.
//! - **`Castling`**: A struct representing castling rights for both players using bit flags.
//!   - Each bit corresponds to a specific castling right (White Kingside, White Queenside, Black Kingside, Black Queenside).
//!   - Implements bitwise operations for easy manipulation (AND, OR, XOR, NOT).
//!   - Supports checking, adding, and removing castling rights.
//!   - Compact representation using a single byte.
//!   - Helper methods: `has()`, `set()`, and `remove()`.
//!
//! ## Usage
//!
//! These types are used throughout the chess engine to represent the state of the game,
//! generate moves, and evaluate board positions. They are designed to be efficient and easy to use.
use super::errors::SquareAddError;
use super::{File, Square};
use macros::{BitOps, EnumIter, FromPrimitive};

/******************************************\
|==========================================|
|                 Colours                  |
|==========================================|
\******************************************/

/// # Colour Representation
/// 
/// Represents the two colours in chess: White and Black.
/// 
/// ```rust,no_run
/// use sophos::Colour;
/// 
/// const WHITE: bool = false;
/// const BLACK: bool = true;
/// ```
/// 
/// ## Features
/// - Implements `From<u8>` for numeric conversion
/// - Supports iteration with `Colour::iter()`
/// - Length variable: `Colour::NUM = 2`

#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter, FromPrimitive)]
pub enum Colour {
    // #[default]
    White, 
    Black
}

/******************************************\
|==========================================|
|                 Direction                |
|==========================================|
\******************************************/

/// # Direction Representation
///
/// Represents movement directions on a chess board. Each direction
/// corresponds to the index difference between squares when moving in that direction.
///
/// ```rust,no_run
/// use sophos::Direction;
/// 
/// // Direction values
/// const N: i8 = 8;
/// const S: i8 = -8;
/// const W: i8 = -1;
/// const E: i8 = 1;
/// // ... other directions
/// ```
///
/// ## Features
/// - Provides all common chess movement directions
/// - Implements `From<i8>` for numeric conversion
/// - Semantic directional naming (N = North, S = South, etc.)
/// 
/// ## Implementation
/// Directions can be applied to squares using the `+` operator, which returns
/// an `Option<Square>`. The result is `None` if the move would go off the board:
/// 
/// ```rust,no_run
/// use sophos::{Square, Direction};
/// 
/// let e4 = Square::E4;
/// let e5 = e4.add(Direction::N;  // Some(Square::E5)
/// let off_board = Square::H1.add(Direction::E;  // None - would go off the board
/// ```
/// 
/// You can also convert between squares to find the direction using `TryFrom`:
/// 
/// ```rust,no_run
/// use sophos::{Square, Direction};
/// 
/// let dir = Direction::try_from(Square::E4, Square::E5);  // Ok(Direction::N)
/// let invalid = Direction::try_from(Square::E4, Square::G5);  // Err - not a valid direction
/// ```
/// 
/// Only adjacent squares in the 8 principal directions are considered valid directions.
#[rustfmt::skip]
#[repr(i8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromPrimitive)]
pub enum Direction {
    N = 8, S = -8, W = -1, E = 1,
    NE = 9, NW = 7, SE = -7, SW = -9,
    NNE = 17, NNW = 15, NEE = 10, NWW = 6, 
    SEE = -6, SWW = -10, SSE = -15, SSW = -17,
    NN = 16, SS = -16,
}

/******************************************\
|==========================================|
|                 Castling                 |
|==========================================|
\******************************************/

/// # Castling Rights Representation
///
/// Represents the castling rights for both players using bit flags.
/// Each bit corresponds to a specific castling right.
///
/// ```rust,no_run
/// use sophos::Castling;
///
/// // Castling flags
/// const WHITE_KINGSIDE: u8 = 0b0001;  // 1
/// const WHITE_QUEENSIDE: u8 = 0b0010;  // 2
/// const BLACK_KINGSIDE: u8 = 0b0100;  // 4
/// const BLACK_QUEENSIDE: u8 = 0b1000;  // 8
/// const ALL_CASTLING: u8 = 0b1111;  // 15
/// const NO_CASTLING: u8 = 0b0000;  // 0
/// ```
///
/// ## Features
/// - Implements bitwise operations for easy manipulation (AND, OR, XOR, NOT)
/// - Supports checking, adding, and removing castling rights
/// - Compact representation using a single byte
/// - Helper methods: `is_set()`, `set()`, and `remove()`
///
/// ## Implementation
/// Castling rights can be manipulated using bitwise operations:
///
/// ```rust,no_run
/// // Initialize with all castling rights
/// let mut castling = Castling::ALL;
///
/// // Remove White Kingside castling
/// castling &= !Castling::WK;
///
/// // Check if Black Queenside castling is allowed
/// if castling & Castling::BQ != Castling(0) {
///     println!("Black can castle queenside");
/// }
///
/// // Add White Queenside castling
/// castling |= Castling::WQ;
///
/// // Toggle all castling rights
/// castling ^= Castling::ALL;
/// ```
///
/// Castling rights can also be manipulated using helper methods:
///
/// ```rust,no_run
/// // Check if Black Kingside castling is allowed
/// if castling.is_set(Castling::BK) {
///     println!("Black can castle kingside");
/// }
///
/// // Add White Queenside castling
/// castling.set(Castling::WQ);
///
/// // Remove Black Queenside castling
/// castling.remove(Castling::BQ);
/// ```
///
/// Castling rights are updated during game play when:
/// - A king moves (losing all castling rights for that side)
/// - A rook moves (losing the corresponding castling right)
/// - A rook is captured (opponent loses the corresponding castling right)
#[derive(Debug, Clone, Copy, PartialEq, Eq, BitOps)]
pub struct Castling(pub u8);

impl Default for Castling {
    fn default() -> Self {
        Castling::ALL
    }
}

/******************************************\
|==========================================|
|              Implementation              |
|==========================================|
\******************************************/

impl Colour {
    pub const NUM: usize = 2;
    /// Return forward direction for square
    pub const fn forward(&self) -> Direction {
        match self {
            Colour::White => Direction::N,
            Colour::Black => Direction::S,
        }
    }
}

impl std::ops::Not for Colour {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
        }
    }
}

// Allow converting from i8 to Square, with bounds checking
impl Square {
    pub const fn try_from(value: i16) -> Result<Self, &'static str> {
        if value >= 0 && value < 64 {
            Ok(unsafe { Square::from_unchecked(value as u8) })
        } else {
            Err("Square value out of bounds (0-63)")
        }
    }

    pub const fn add(self, rhs: Direction) -> Result<Self, SquareAddError> {
        // Get file and rank for bounds checking
        let file = self.file() as u8;

        // Check bounds based on direction
        use Direction::*;
        let valid = match rhs {
            N | S | NN | SS => true,
            E | NE | NNE | SE | SSE if file < File::FileH as u8 => true,
            W | NW | NNW | SW | SSW if file > File::FileA as u8 => true,
            NEE | SEE if file < File::FileG as u8 => true,
            NWW | SWW if file > File::FileB as u8 => true,
            _ => false,
        };

        match valid {
            true => match Square::try_from(self as i16 + rhs as i16) {
                Ok(sq) => Ok(sq),
                Err(_) => return Err(SquareAddError::OutOfBounds),
            },
            false => return Err(SquareAddError::OutOfBounds),
        }
    }
}

// Negating a direction reverses it
impl std::ops::Neg for Direction {
    type Output = Self;

    fn neg(self) -> Self::Output {
        unsafe { Self::from_unchecked(-(self as i8)) }
    }
}

// Convert two squares into a direction
impl Direction {
    /// Tries to convert two squares into a direction.
    ///
    /// This function attempts to find a valid chess direction between two given squares.
    /// It only considers the 8 principal directions (N, S, E, W, NE, NW, SE, SW).
    ///
    /// # Arguments
    ///
    /// * `from` - The starting square.
    /// * `to` - The destination square.
    ///
    /// # Returns
    ///
    /// * `Ok(Direction)` - The direction from `from` to `to`.
    /// * `Err(&'static str)` - If the squares are the same or if there is no valid direction between them.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Square, Direction};
    ///
    /// let dir = Direction::try_from(Square::E4, Square::E5); // Ok(Direction::N)
    /// let invalid = Direction::try_from(Square::E4, Square::G5); // Err - not a valid direction
    /// ```
    ///
    /// # Errors
    ///
    /// This function will return an error in the following cases:
    ///
    /// * The `from` and `to` squares are the same.
    /// * The `to` square is not reachable from the `from` square in a single move in any of the 8 principal directions.
    /// * The move is not a valid chess direction.
    ///
    /// # Notes
    ///
    /// This function only considers the 8 principal directions (N, S, E, W, NE, NW, SE, SW).
    /// It does not consider knight moves or other non-standard moves.
    /// It also does not consider board wrapping.

    pub const fn try_from(from: Square, to: Square) -> Result<Self, &'static str> {
        if from as u8 == to as u8 {
            return Err("Squares are the same");
        }

        // Calculate rank and file distances
        let rank_dist = (to.rank() as i8) - (from.rank() as i8);
        let file_dist = (to.file() as i8) - (from.file() as i8);

        // Determine the direction based on rank and file distances
        match (rank_dist, file_dist) {
            // Cardinal directions
            (0, i) if i < 0 => Ok(Direction::W),
            (0, i) if i > 0 => Ok(Direction::E),
            (i, 0) if i > 0 => Ok(Direction::N),
            (i, 0) if i < 0 => Ok(Direction::S),

            // Diagonal directions
            (i, j) if i == j && i < 0 => Ok(Direction::SW),
            (i, j) if i == j && i > 0 => Ok(Direction::NE),
            (i, j) if i == -j && i > 0 => Ok(Direction::NW),
            (i, j) if i == -j && i < 0 => Ok(Direction::SE),

            // Non-standard or illegal moves
            _ => Err("No valid direction between these squares"),
        }
    }
}

impl Castling {
    /// ### White King Side
    pub const WK: Castling = Castling(1);
    /// ### White Queen Side
    pub const WQ: Castling = Castling(2);
    /// ### Black King Side
    pub const BK: Castling = Castling(4);
    /// ### Black Queen Side
    pub const BQ: Castling = Castling(8);

    /// ### King Side
    pub const KING_SIDE: Castling = Castling(5);
    /// ### Queen Side
    pub const QUEEN_SIDE: Castling = Castling(10);

    /// ### White Castling
    pub const WHITE_CASTLING: Castling = Castling(3);
    /// ### Black Castling
    pub const BLACK_CASTLING: Castling = Castling(12);

    /// ### All Castling Rights
    pub const ALL: Castling = Castling(15);

    /// ### No Castling Rights
    pub const NONE: Castling = Castling(0);

    /// ### Number of castling right combinations
    pub const NUM: usize = 16;

    /// ### Check if a castling right is set
    pub const fn has(self, right: Castling) -> bool {
        self.bitand(right).0 != Castling::NONE.0
    }

    /// ### Set a castling right
    pub const fn set(&mut self, right: Castling) {
        self.bitor_assign(right);
    }

    /// ### Remove a castling right
    pub const fn remove(&mut self, right: Castling) {
        self.bitand_assign(right.not());
    }
    /// ### Constant version of not operator !
    #[inline]
    pub const fn not(self) -> Self {
        Castling(!self.0 & 0x0F) // Only keep the lower 4 bits
    }

    /// ### Mask the castling rights
    #[inline]
    pub const fn mask(&mut self, mask: Castling) {
        self.0 &= mask.0;
    }
}

// Override the Not implementation to only affect the lower 4 bits
impl std::ops::Not for Castling {
    type Output = Self;

    #[inline]
    fn not(self) -> Self::Output {
        Castling(!self.0 & 0x0F) // Only keep the lower 4 bits
    }
}

impl std::fmt::Display for Castling {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 0 {
            return write!(f, "-");
        }

        let mut s = String::new();
        if self.has(Castling::WK) {
            s.push('K');
        }
        if self.has(Castling::WQ) {
            s.push('Q');
        }
        if self.has(Castling::BK) {
            s.push('k');
        }
        if self.has(Castling::BQ) {
            s.push('q');
        }

        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::square::Square;

    #[test]
    fn test_adjacent_cardinal_directions() {
        // Test adjacent cardinal directions (N, S, E, W)
        // These should still work with the new logic.
        assert_eq!(
            Direction::try_from(Square::E4, Square::E5),
            Ok(Direction::N)
        );
        assert_eq!(
            Direction::try_from(Square::E4, Square::E3),
            Ok(Direction::S)
        );
        assert_eq!(
            Direction::try_from(Square::E4, Square::F4),
            Ok(Direction::E)
        );
        assert_eq!(
            Direction::try_from(Square::E4, Square::D4),
            Ok(Direction::W)
        );
    }

    #[test]
    fn test_adjacent_diagonal_directions() {
        // Test adjacent diagonal directions (NE, NW, SE, SW)
        // These should also still work.
        assert_eq!(
            Direction::try_from(Square::E4, Square::F5),
            Ok(Direction::NE)
        );
        assert_eq!(
            Direction::try_from(Square::E4, Square::D5),
            Ok(Direction::NW)
        );
        assert_eq!(
            Direction::try_from(Square::E4, Square::F3),
            Ok(Direction::SE)
        );
        assert_eq!(
            Direction::try_from(Square::E4, Square::D3),
            Ok(Direction::SW)
        );
    }

    #[test]
    fn test_non_adjacent_directions() {
        // Test non-adjacent squares on the same line
        // Ranks
        assert_eq!(
            Direction::try_from(Square::A4, Square::H4), // User's example
            Ok(Direction::E)
        );
        assert_eq!(
            Direction::try_from(Square::H4, Square::A4),
            Ok(Direction::W)
        );
        assert_eq!(
            Direction::try_from(Square::C1, Square::G1),
            Ok(Direction::E)
        );
        assert_eq!(
            Direction::try_from(Square::G8, Square::C8),
            Ok(Direction::W)
        );

        // Files
        assert_eq!(
            Direction::try_from(Square::A1, Square::A8),
            Ok(Direction::N)
        );
        assert_eq!(
            Direction::try_from(Square::A8, Square::A1),
            Ok(Direction::S)
        );
        assert_eq!(
            Direction::try_from(Square::H3, Square::H7),
            Ok(Direction::N)
        );
        assert_eq!(
            Direction::try_from(Square::D6, Square::D2),
            Ok(Direction::S)
        );

        // Diagonals (A1-H8 type)
        assert_eq!(
            Direction::try_from(Square::A1, Square::H8),
            Ok(Direction::NE)
        );
        assert_eq!(
            Direction::try_from(Square::H8, Square::A1),
            Ok(Direction::SW)
        );
        assert_eq!(
            Direction::try_from(Square::C3, Square::F6),
            Ok(Direction::NE)
        );
        assert_eq!(
            Direction::try_from(Square::F6, Square::C3),
            Ok(Direction::SW)
        );

        // Diagonals (A8-H1 type)
        assert_eq!(
            Direction::try_from(Square::A8, Square::H1),
            Ok(Direction::SE)
        );
        assert_eq!(
            Direction::try_from(Square::H1, Square::A8),
            Ok(Direction::NW)
        );
        assert_eq!(
            Direction::try_from(Square::B7, Square::E4),
            Ok(Direction::SE)
        );
        assert_eq!(
            Direction::try_from(Square::E4, Square::B7),
            Ok(Direction::NW)
        );
    }

    #[test]
    fn test_invalid_directions() {
        // Test same square (should still be invalid)
        assert!(Direction::try_from(Square::E4, Square::E4).is_err());

        // Test non-directional moves (like knight moves - should still be invalid)
        assert!(Direction::try_from(Square::E4, Square::F6).is_err());
        assert!(Direction::try_from(Square::E4, Square::G5).is_err());

        // Test squares not on the same rank, file, or diagonal
        assert!(Direction::try_from(Square::A1, Square::B3).is_err());
        assert!(Direction::try_from(Square::H8, Square::F5).is_err());
    }

    // test_board_edges remains valid as it tests adjacent squares
    #[test]
    fn test_board_edges() {
        // Test directions at the edges of the board (adjacent)
        // Top edge
        assert_eq!(
            Direction::try_from(Square::A8, Square::B8),
            Ok(Direction::E)
        );
        // Bottom edge
        assert_eq!(
            Direction::try_from(Square::H1, Square::G1),
            Ok(Direction::W)
        );
        // Left edge
        assert_eq!(
            Direction::try_from(Square::A4, Square::A5),
            Ok(Direction::N)
        );
        // Right edge
        assert_eq!(
            Direction::try_from(Square::H4, Square::H3),
            Ok(Direction::S)
        );
        // Corners
        assert_eq!(
            Direction::try_from(Square::A1, Square::B2),
            Ok(Direction::NE)
        );
        assert_eq!(
            Direction::try_from(Square::H1, Square::G2),
            Ok(Direction::NW)
        );
        assert_eq!(
            Direction::try_from(Square::A8, Square::B7),
            Ok(Direction::SE)
        );
        assert_eq!(
            Direction::try_from(Square::H8, Square::G7),
            Ok(Direction::SW)
        );
    }

    // test_board_wrapping_edge_cases remains valid as these are not on the same line
    #[test]
    fn test_board_wrapping_edge_cases() {
        // These moves look like valid directions by raw index difference,
        // but are invalid because they wrap around the board OR are not on the same line
        assert!(Direction::try_from(Square::H4, Square::A5).is_err()); // Not on same line
        assert!(Direction::try_from(Square::H4, Square::A3).is_err()); // Not on same line
    }

    #[test]
    fn test_square_plus_direction() {
        // Test cardinal directions
        assert_eq!(Square::E4.add(Direction::N), Ok(Square::E5));
        assert_eq!(Square::E4.add(Direction::S), Ok(Square::E3));
        assert_eq!(Square::E4.add(Direction::E), Ok(Square::F4));
        assert_eq!(Square::E4.add(Direction::W), Ok(Square::D4));

        // Test diagonal directions
        assert_eq!(Square::E4.add(Direction::NE), Ok(Square::F5));
        assert_eq!(Square::E4.add(Direction::NW), Ok(Square::D5));
        assert_eq!(Square::E4.add(Direction::SE), Ok(Square::F3));
        assert_eq!(Square::E4.add(Direction::SW), Ok(Square::D3));

        // Test edge cases
        assert_eq!(
            Square::H4.add(Direction::E),
            Err(SquareAddError::OutOfBounds)
        ); // Off board to the right
        assert_eq!(
            Square::A4.add(Direction::W),
            Err(SquareAddError::OutOfBounds)
        ); // Off board to the left
        assert_eq!(
            Square::E8.add(Direction::N),
            Err(SquareAddError::OutOfBounds)
        ); // Off board to the top
        assert_eq!(
            Square::E1.add(Direction::S),
            Err(SquareAddError::OutOfBounds)
        ); // Off board to the bottom

        // Test knight moves
        assert_eq!(Square::E4.add(Direction::NNE), Ok(Square::F6));
        assert_eq!(Square::E4.add(Direction::NEE), Ok(Square::G5));

        // Test double moves
        assert_eq!(Square::E4.add(Direction::NN), Ok(Square::E6));
        assert_eq!(Square::E4.add(Direction::SS), Ok(Square::E2));

        // Test edge cases with knight moves
        assert_eq!(
            Square::H7.add(Direction::NEE),
            Err(SquareAddError::OutOfBounds)
        ); // Would go off the board
        assert_eq!(
            Square::A2.add(Direction::SWW),
            Err(SquareAddError::OutOfBounds)
        ); // Would go off the board

        // Test all directions
        use Direction::*;

        let directions: [Direction; 18] = [
            N, S, E, W, NE, NW, SE, SW, NNE, NNW, NEE, NWW, SSE, SSW, SEE, SWW, NN, SS,
        ];

        for dir in directions {
            for sq in Square::iter() {
                match sq.add(dir) {
                    Ok(new_sq) => assert_eq!(new_sq.add(-dir), Ok(sq)),
                    Err(err) => assert_eq!(err, SquareAddError::OutOfBounds),
                }
            }
        }
    }

    #[test]
    fn test_tryfrom_i16_for_square() {
        // Valid conversions
        assert_eq!(Square::try_from(0i16), Ok(Square::A1));
        assert_eq!(Square::try_from(36i16), Ok(Square::E5));
        assert_eq!(Square::try_from(63i16), Ok(Square::H8));

        // Invalid conversions
        assert!(Square::try_from(-1i16).is_err());
        assert!(Square::try_from(64i16).is_err());
    }

    #[test]
    fn test_castling_bitwise_operations() {
        // Test bitwise operations
        let all = Castling::ALL;
        let none = Castling::NONE;
        let wk = Castling::WK;
        let bq = Castling::BQ;

        // Test bitwise AND
        assert_eq!(all & wk, wk);
        assert_eq!(none & all, none);

        // Test bitwise OR
        assert_eq!(wk | bq, Castling(9)); // 1 | 8 = 9
        assert_eq!(none | wk, wk);

        // Test bitwise XOR
        assert_eq!(all ^ none, all);
        assert_eq!(all ^ all, none);
        assert_eq!(wk ^ bq, Castling(9)); // 1 ^ 8 = 9

        // Test NOT
        assert_eq!(!none, all);
        assert_eq!(!all, none);
        assert_eq!(!wk, Castling(14)); // NOT 0001 = 1110
    }

    #[test]
    fn test_castling_helper_methods() {
        let mut castling = Castling::ALL;

        // Test is_set
        assert!(castling.has(Castling::WK));
        assert!(castling.has(Castling::WQ));
        assert!(castling.has(Castling::BK));
        assert!(castling.has(Castling::BQ));

        // Test remove
        castling.remove(Castling::WK);
        assert!(!castling.has(Castling::WK));
        assert!(castling.has(Castling::WQ));
        assert!(castling.has(Castling::BK));
        assert!(castling.has(Castling::BQ));

        // Test set
        castling = Castling::NONE;
        assert!(!castling.has(Castling::WK));
        castling.set(Castling::WK);
        assert!(castling.has(Castling::WK));

        // Test compound rights
        castling = Castling::NONE;
        castling.set(Castling::WHITE_CASTLING);
        assert!(castling.has(Castling::WK));
        assert!(castling.has(Castling::WQ));
        assert!(!castling.has(Castling::BK));
        assert!(!castling.has(Castling::BQ));

        castling = Castling::ALL;
        castling.remove(Castling::BLACK_CASTLING);
        assert!(castling.has(Castling::WK));
        assert!(castling.has(Castling::WQ));
        assert!(!castling.has(Castling::BK));
        assert!(!castling.has(Castling::BQ));
    }
}
