use super::{File, Square};
use thiserror::Error;

/******************************************\
|==========================================|
|                 Colours                  |
|==========================================|
\******************************************/

/// # Colour Representation
/// 
/// Represents the two colours in chess: White and Black.

#[rustfmt::skip]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Colour {
    White, 
    Black
}

impl Colour {
    /// Number of elements in the Colour enum
    pub const NUM: usize = 2;
}

crate::impl_from_to_primitive!(Colour);

/******************************************\
|==========================================|
|                 Direction                |
|==========================================|
\******************************************/

/// # Direction Representation
/// 
/// Represents the 8 directions in chess, plus special directions for knightmoves and pawn double pushes

#[rustfmt::skip]
#[repr(i8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    N = 8, S = -8, W = -1, E = 1,
    NE = 9, NW = 7, SE = -7, SW = -9,
    NNE = 17, NNW = 15, NEE = 10, NWW = 6, 
    SEE = -6, SWW = -10, SSE = -15, SSW = -17,
    NN = 16, SS = -16,
}

crate::impl_from_to_primitive!(Direction, i8);

/******************************************\
|==========================================|
|                 Castling                 |
|==========================================|
\******************************************/

/// # Castling Representation
///
/// Represents the castling rights for a position

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Castling(pub u8);

impl Default for Castling {
    fn default() -> Self {
        Castling::ALL
    }
}

crate::impl_bit_ops!(Castling);

/******************************************\
|==========================================|
|              Implementation              |
|==========================================|
\******************************************/

impl Colour {
    /// Returns the forward direction for a colour
    pub const fn forward(&self) -> Direction {
        match self {
            Colour::White => Direction::N,
            Colour::Black => Direction::S,
        }
    }

    /// Returns the forward left direction (pawn capture) for a colour
    pub const fn forward_left(&self) -> Direction {
        match self {
            Colour::White => Direction::NE,
            Colour::Black => Direction::SW,
        }
    }

    /// Returns the forward right direction (pawn capture) for a colour
    pub const fn forward_right(&self) -> Direction {
        match self {
            Colour::White => Direction::NW,
            Colour::Black => Direction::SE,
        }
    }

    /// Returns the double forward direction (pawn double push) for a colour
    pub const fn double_forward(&self) -> Direction {
        match self {
            Colour::White => Direction::NN,
            Colour::Black => Direction::SS,
        }
    }
}

impl std::ops::Not for Colour {
    type Output = Self;

    /// Returns the opposite colour
    fn not(self) -> Self::Output {
        match self {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
        }
    }
}

impl Square {
    /// Try to convert from i16 to a square (Returns error if out of bounds)
    pub const fn try_from(value: i16) -> Result<Self, &'static str> {
        if value >= 0 && value < 64 {
            Ok(Square::from_unchecked(value as u8))
        } else {
            Err("Square value out of bounds (0-63)")
        }
    }

    /// Try to add direction to a square
    #[inline]
    pub const fn add(self, rhs: Direction) -> Result<Self, SquareAddError> {
        let file = self.file() as u8;

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

    /// Add direction to a square without checking
    #[inline]
    pub const unsafe fn add_unchecked(self, rhs: Direction) -> Self {
        debug_assert!(self as i16 + rhs as i16 >= 0, "Square out of bounds");
        debug_assert!(self as i16 + rhs as i16 <= 64, "Square out of bounds");
        Square::from_unchecked((self as i16 + rhs as i16) as u8)
    }
}

impl std::ops::Neg for Direction {
    type Output = Self;

    /// Negate the direction (N => S, etc...)
    fn neg(self) -> Self::Output {
        Self::from_unchecked(-(self as i8))
    }
}

impl Direction {
    /// Try to get the direction between two squares (From the direction enum)
    pub const fn try_from(from: Square, to: Square) -> Result<Self, &'static str> {
        if from as u8 == to as u8 {
            return Err("Squares are the same");
        }

        let rank_dist = (to.rank() as i8) - (from.rank() as i8);
        let file_dist = (to.file() as i8) - (from.file() as i8);

        match (rank_dist, file_dist) {
            (0, i) if i < 0 => Ok(Direction::W),
            (0, i) if i > 0 => Ok(Direction::E),
            (i, 0) if i > 0 => Ok(Direction::N),
            (i, 0) if i < 0 => Ok(Direction::S),

            (i, j) if i == j && i < 0 => Ok(Direction::SW),
            (i, j) if i == j && i > 0 => Ok(Direction::NE),
            (i, j) if i == -j && i > 0 => Ok(Direction::NW),
            (i, j) if i == -j && i < 0 => Ok(Direction::SE),

            _ => Err("No valid direction between these squares"),
        }
    }
}

impl Castling {
    // Number of possible elements in the Castling struct
    pub const NUM: usize = 16;
    // Atomic castling rights
    pub const WK: Castling = Castling(1);
    pub const WQ: Castling = Castling(2);
    pub const BK: Castling = Castling(4);
    pub const BQ: Castling = Castling(8);
    // Board side castling rights
    pub const KING_SIDE: Castling = Castling(5);
    pub const QUEEN_SIDE: Castling = Castling(10);
    // Board colour castling rights
    pub const WHITE_CASTLING: Castling = Castling(3);
    pub const BLACK_CASTLING: Castling = Castling(12);
    // All or nothing castling rights
    pub const ALL: Castling = Castling(15);
    pub const NONE: Castling = Castling(0);

    /// Helper function to check if a castling right has another castling right as a subset
    pub fn has(self, right: Castling) -> bool {
        self & right != Castling::NONE
    }

    /// Helper function to set castling rights
    pub fn set(&mut self, right: Castling) {
        *self |= right;
    }

    /// Helper function to remove castling rights
    pub fn remove(&mut self, right: Castling) {
        *self &= !right;
    }

    /// Mask the castling rights using `mask`
    #[inline]
    pub fn mask(&mut self, mask: Castling) {
        self.0 &= mask.0;
    }

    /// Get the king side castling right for a colour
    #[inline]
    pub fn king_side(colour: Colour) -> Self {
        match colour {
            Colour::White => Castling::WK,
            Colour::Black => Castling::BK,
        }
    }

    /// Get the queen side castling right for a colour
    #[inline]
    pub fn queen_side(colour: Colour) -> Self {
        match colour {
            Colour::White => Castling::WQ,
            Colour::Black => Castling::BQ,
        }
    }
}

impl std::ops::Not for Castling {
    type Output = Self;

    /// Invert the bits to give the opposite castling rights
    #[inline]
    fn not(self) -> Self::Output {
        Castling(!self.0 & 0x0F)
    }
}

impl std::fmt::Display for Castling {
    /// Displays castling right in the `KQkq` format
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

/******************************************\
|==========================================|
|             Square Add Errors            |
|==========================================|
\******************************************/

#[derive(Error, Debug, Clone, Copy, PartialEq, Eq)]
pub enum SquareAddError {
    #[error("Square operation resulted in an out-of-bounds position")]
    OutOfBounds,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::square::Square;

    #[test]
    fn test_adjacent_cardinal_directions() {
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
        assert_eq!(
            Direction::try_from(Square::A4, Square::H4),
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
        assert!(Direction::try_from(Square::E4, Square::E4).is_err());

        assert!(Direction::try_from(Square::E4, Square::F6).is_err());
        assert!(Direction::try_from(Square::E4, Square::G5).is_err());

        assert!(Direction::try_from(Square::A1, Square::B3).is_err());
        assert!(Direction::try_from(Square::H8, Square::F5).is_err());
    }

    #[test]
    fn test_board_edges() {
        assert_eq!(
            Direction::try_from(Square::A8, Square::B8),
            Ok(Direction::E)
        );

        assert_eq!(
            Direction::try_from(Square::H1, Square::G1),
            Ok(Direction::W)
        );

        assert_eq!(
            Direction::try_from(Square::A4, Square::A5),
            Ok(Direction::N)
        );

        assert_eq!(
            Direction::try_from(Square::H4, Square::H3),
            Ok(Direction::S)
        );

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

    #[test]
    fn test_board_wrapping_edge_cases() {
        assert!(Direction::try_from(Square::H4, Square::A5).is_err());
        assert!(Direction::try_from(Square::H4, Square::A3).is_err());
    }

    #[test]
    fn test_square_plus_direction() {
        assert_eq!(Square::E4.add(Direction::N), Ok(Square::E5));
        assert_eq!(Square::E4.add(Direction::S), Ok(Square::E3));
        assert_eq!(Square::E4.add(Direction::E), Ok(Square::F4));
        assert_eq!(Square::E4.add(Direction::W), Ok(Square::D4));

        assert_eq!(Square::E4.add(Direction::NE), Ok(Square::F5));
        assert_eq!(Square::E4.add(Direction::NW), Ok(Square::D5));
        assert_eq!(Square::E4.add(Direction::SE), Ok(Square::F3));
        assert_eq!(Square::E4.add(Direction::SW), Ok(Square::D3));

        assert_eq!(
            Square::H4.add(Direction::E),
            Err(SquareAddError::OutOfBounds)
        );
        assert_eq!(
            Square::A4.add(Direction::W),
            Err(SquareAddError::OutOfBounds)
        );
        assert_eq!(
            Square::E8.add(Direction::N),
            Err(SquareAddError::OutOfBounds)
        );
        assert_eq!(
            Square::E1.add(Direction::S),
            Err(SquareAddError::OutOfBounds)
        );

        assert_eq!(Square::E4.add(Direction::NNE), Ok(Square::F6));
        assert_eq!(Square::E4.add(Direction::NEE), Ok(Square::G5));

        assert_eq!(Square::E4.add(Direction::NN), Ok(Square::E6));
        assert_eq!(Square::E4.add(Direction::SS), Ok(Square::E2));

        assert_eq!(
            Square::H7.add(Direction::NEE),
            Err(SquareAddError::OutOfBounds)
        );
        assert_eq!(
            Square::A2.add(Direction::SWW),
            Err(SquareAddError::OutOfBounds)
        );

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
        assert_eq!(Square::try_from(0i16), Ok(Square::A1));
        assert_eq!(Square::try_from(36i16), Ok(Square::E5));
        assert_eq!(Square::try_from(63i16), Ok(Square::H8));

        assert!(Square::try_from(-1i16).is_err());
        assert!(Square::try_from(64i16).is_err());
    }

    #[test]
    fn test_castling_bitwise_operations() {
        let all = Castling::ALL;
        let none = Castling::NONE;
        let wk = Castling::WK;
        let bq = Castling::BQ;

        assert_eq!(all & wk, wk);
        assert_eq!(none & all, none);

        assert_eq!(wk | bq, Castling(9));
        assert_eq!(none | wk, wk);

        assert_eq!(all ^ none, all);
        assert_eq!(all ^ all, none);
        assert_eq!(wk ^ bq, Castling(9));

        assert_eq!(!none, all);
        assert_eq!(!all, none);
        assert_eq!(!wk, Castling(14));
    }

    #[test]
    fn test_castling_helper_methods() {
        let mut castling = Castling::ALL;

        assert!(castling.has(Castling::WK));
        assert!(castling.has(Castling::WQ));
        assert!(castling.has(Castling::BK));
        assert!(castling.has(Castling::BQ));

        castling.remove(Castling::WK);
        assert!(!castling.has(Castling::WK));
        assert!(castling.has(Castling::WQ));
        assert!(castling.has(Castling::BK));
        assert!(castling.has(Castling::BQ));

        castling = Castling::NONE;
        assert!(!castling.has(Castling::WK));
        castling.set(Castling::WK);
        assert!(castling.has(Castling::WK));

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
