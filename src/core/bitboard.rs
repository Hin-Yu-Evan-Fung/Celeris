//! # Module: `bitboard`
//!
//! This module defines the `Bitboard` struct and its associated methods for representing and
//! manipulating chess board states using bitwise operations. Bitboards are a highly efficient
//! way to represent sets of squares on a chess board, allowing for fast calculations and
//! operations.
//!
//! ## Overview
//!
//! A `Bitboard` is a 64-bit integer where each bit corresponds to a square on the chess board.
//! This representation enables the use of bitwise operations to perform set operations on
//! squares, such as union, intersection, and difference. It also allows for efficient
//! checking of whether a square is occupied or not.
//!
//! ## Key Components
//!
//! - **`Bitboard`**: A struct representing a bitboard.
//!   - Uses a `u64` to store the bitboard data.
//!   - Implements bitwise operations (AND, OR, XOR, NOT, SHL, SHR) for efficient manipulation.
//!   - Provides methods for setting, clearing, and toggling individual bits (squares).
//!   - Includes methods for finding the least significant bit (LSB) and most significant bit (MSB).
//!   - Supports iteration over set bits using `for_each()`.
//!   - Offers methods for counting set bits (`count_bits()`) and checking if the bitboard is empty (`is_empty()`).
//!   - Implements `Display` for printing the bitboard in a visual chess board format.
//!   - Provides constants for common bitboard patterns (e.g., `EMPTY`, `A1`, `RANK_1`, `FILE_A`).
//!   - Implements `From` for converting from `Square`, `Rank`, and `File` to `Bitboard`.
//!   - Implements `shift()` for moving bits in chess directions.
//!
//! ## Functionality
//!
//! - **Bitwise Operations**: `Bitboard` supports all standard bitwise operations, allowing for
//!   complex set manipulations.
//! - **Square Manipulation**: Methods like `set()`, `clear()`, and `toggle()` allow for easy
//!   modification of individual squares.
//! - **Bit Examination**: `lsb()` and `msb()` provide a way to find the first and last set bits,
//!   respectively. `pop_

use std::fmt;

use super::{Direction, File, Rank, Square};
use macros::{AriOps, BitManiOps, BitOps};

/// # Bitboard Representation
///
/// Represents a chess board using a 64-bit integer, where each bit corresponds to a square.
/// This allows for efficient board representation and operations using bitwise logic.
///
/// ## Visual Representation
///
/// ```text
///     +---+---+---+---+---+---+---+---+
/// 8   | * | * | * | * | * | * | * | * |    // * means empty square
///     +---+---+---+---+---+---+---+---+    // 1 means set bit
/// 7   | * | * | * | * | * | * | * | * |
///     +---+---+---+---+---+---+---+---+
/// 6   | * | * | * | * | * | * | * | * |
///     +---+---+---+---+---+---+---+---+
/// 5   | * | * | * | * | * | * | * | * |
///     +---+---+---+---+---+---+---+---+
/// 4   | * | * | * | * | * | * | * | * |
///     +---+---+---+---+---+---+---+---+
/// 3   | * | * | * | * | * | * | * | * |
///     +---+---+---+---+---+---+---+---+
/// 2   | 1 | 1 | * | * | * | * | * | * |
///     +---+---+---+---+---+---+---+---+
/// 1   | 1 | * | * | 1 | * | 1 | 1 | 1 |
///     +---+---+---+---+---+---+---+---+
///       A   B   C   D   E   F   G   H
///
/// Bitboard: 0x3e9
/// ```
///
/// ## Features
/// - Implements bitwise operations (AND, OR, XOR, NOT, SHL, SHR)
/// - Conversions from Square, Rank, and File
/// - Square manipulation: `set()`, `clear()`, `toggle()`, `get()`
/// - Bit examination: `lsb()`, `msb()`, `pop_lsb()`, `pop_msb()`
/// - Iteration: `for_each()`
/// - Counting: `count_bits()`, `is_empty()`
/// - Board movement: `shift()` for moving bits in chess directions
/// - Display: Pretty-printing of bitboards in a visual chess board format
///
/// ## Constants
/// - `EMPTY`: Empty bitboard (no bits set)
/// - `A1`: Bitboard with only the A1 square set
/// - `RANK_1`: Bitboard with all squares in rank 1 set
/// - `FILE_A`: Bitboard with all squares in file A set
///
/// ## Available Functions
/// ```rust,no_run
/// // Square manipulation
/// bitboard.set(square)    // Set a bit at the specified square
/// bitboard.clear(square)  // Clear a bit at the specified square
/// bitboard.toggle(square) // Toggle a bit at the specified square
/// bitboard.get(square)    // Check if a bit is set at the specified square
///
/// // Bit examination
/// bitboard.lsb()      // Get the least significant bit (lowest set square)
/// bitboard.msb()      // Get the most significant bit (highest set square)
/// bitboard.pop_lsb()  // Get and remove the least significant bit
/// bitboard.pop_msb()  // Get and remove the most significant bit
///
/// // Counting
/// bitboard.count_bits() // Count the number of bits set
/// bitboard.is_empty()   // Check if the bitboard is empty
///
/// // Iteration
/// bitboard.for_each(|square| {}) // Apply a function to each set square
///
/// // Movement
/// bitboard.shift(direction)   // Shift the bitboard in a chess direction (N, S, E, W, etc.)
/// ```
///
/// ## Implementation
/// Bitboards are manipulated using bitwise operations:
///
/// ```rust,no_run
/// // Create an empty bitboard
/// let empty = Bitboard::EMPTY;
///
/// // Create a bitboard with a single square set
/// let e4_bitboard = Bitboard::from(Square::E4);
///
/// // Create a bitboard for a full rank
/// let rank_2 = Bitboard::from(Rank::Rank2);
///
/// // Check if a square is occupied
/// if e4_bitboard.get(Square::E4) {
///     println!("E4 is occupied");
/// }
///
/// // Set a square
/// let mut bitboard = Bitboard::EMPTY;
/// bitboard.set(Square::E4);
///
/// // Combine two bitboards
/// let combined = e4_bitboard | rank_2;
///
/// // Shift a bitboard in a chess direction
/// let moved_north = e4_bitboard.shift(Direction::N);  // Move one square north
/// let moved_knight = e4_bitboard.shift(Direction::NNE); // Knight-like move north-north-east
///
/// // Display the bitboard in a chess board format
/// println!("{}", bitboard);
///
/// // Iterate through all set squares
/// bitboard.for_each(|square| {
///     println!("Square: {:?}", square);
/// });
/// ```
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, BitOps, BitManiOps, AriOps)]
pub struct Bitboard(pub u64);

/******************************************\
|==========================================|
|                Constants                 |
|==========================================|
\******************************************/

impl Bitboard {
    /// Empty bitboard (no bits set)
    pub const EMPTY: Bitboard = Bitboard(0);
    /// Bitboard with only A1 square set
    pub const A1: Bitboard = Bitboard(1);
    /// Bitboard with all squares in rank 1 set
    pub const RANK_1: Bitboard = Bitboard(0x00000000000000ff);
    /// Bitboard with all squares in rank 8 set
    pub const RANK_8: Bitboard = Bitboard(0xff00000000000000);
    /// Bitboard with all squares in rank 2 set
    pub const RANK_12: Bitboard = Bitboard(0x000000000000ffff);
    /// Bitboard with all squares in rank 7 set
    pub const RANK_78: Bitboard = Bitboard(0xffff000000000000);
    /// Bitboard with all squares in file A set
    pub const FILE_A: Bitboard = Bitboard(0x0101010101010101);
    /// Bitboard with all squares in file H set
    pub const FILE_H: Bitboard = Bitboard(0x8080808080808080);
    /// Bitboard with all squares in file AB set
    const FILE_AB: Bitboard = Bitboard(0x303030303030303);
    /// Bitboard with all squares in file GH set
    const FILE_GH: Bitboard = Bitboard(0xC0C0C0C0C0C0C0C0);
}

/******************************************\
|==========================================|
|                Conversions               |
|==========================================|
\******************************************/

impl From<Square> for Bitboard {
    fn from(square: Square) -> Self {
        Bitboard::A1 << square as u8
    }
}

impl From<Rank> for Bitboard {
    fn from(rank: Rank) -> Self {
        Bitboard::RANK_1 << (8 * rank as u8)
    }
}

impl From<File> for Bitboard {
    fn from(file: File) -> Self {
        Bitboard::FILE_A << file as u8
    }
}

impl<const N: usize> From<[Square; N]> for Bitboard {
    /// Creates a `Bitboard` from a slice of `Square`s.
    ///
    /// This function sets the bits corresponding to each square in the input slice.
    ///    /// # Panics
    ///
    /// This function will panic if the input slice contains more than 64 squares.
    ///
    /// # Examples          
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let squares = [Square::E4, Square::D5, Square::A1];
    /// let bb = Bitboard::from(squares);
    ///
    /// assert!(bb.get(Square::E4));
    /// assert!(bb.get(Square::D5));
    /// assert!(bb.get(Square::A1));
    /// assert_eq!(bb.count_bits(), 3);
    ///
    /// ```
    ///
    /// # Arguments
    ///
    /// * `squares` - A slice of `Square`s to set in the bitboard.
    ///
    /// # Returns
    ///
    /// A `Bitboard` with the specified squares set.
    fn from(squares: [Square; N]) -> Bitboard {
        let mut bb = Bitboard::EMPTY;
        for square in squares {
            bb.set(square);
        }
        bb
    }
}

/******************************************\
|==========================================|
|         Bit Manipulation Functions       |
|==========================================|
\******************************************/

impl Bitboard {
    /// Returns the least significant bit (LSB) of the bitboard as a Square
    ///
    /// The LSB is the first (lowest) set bit in the bitboard's binary representation.
    /// This is useful for iterating through pieces or finding the "first" piece on a bitboard.
    ///
    /// # Returns
    ///
    /// * `Some(Square)` - The square corresponding to the least significant bit
    /// * `None` - If the bitboard is empty (no bits set)
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    ///
    /// let e4_bitboard = Bitboard::from(Square::E4);
    /// assert_eq!(e4_bitboard.lsb(), Some(Square::E4));
    /// ```
    #[inline]
    pub fn lsb(&self) -> Option<Square> {
        match self.0 {
            0 => None,
            bits => Some(Square::from(bits.trailing_zeros() as u8)),
        }
    }

    /// Returns the most significant bit (MSB) of the bitboard as a Square
    ///
    /// The MSB is the last (highest) set bit in the bitboard's binary representation.
    /// This is useful for finding the "last" or "highest" piece on a bitboard.
    ///
    /// # Returns
    ///
    /// * `Some(Square)` - The square corresponding to the most significant bit
    /// * `None` - If the bitboard is empty (no bits set)
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    ///
    /// let multi_square = Bitboard::from(Square::E4) | Bitboard::from(Square::G7);
    /// assert_eq!(multi_square.msb(), Some(Square::G7)); // G7 is higher than E4
    ///
    /// let empty = Bitboard::EMPTY;
    /// assert_eq!(empty.msb(), None);
    /// ```
    #[inline]
    pub fn msb(&self) -> Option<Square> {
        match self.0 {
            0 => None,
            bits => Some(Square::from(63 - bits.leading_zeros() as u8)),
        }
    }

    /// Returns the least significant bit (LSB) and removes it from the bitboard
    ///
    /// This is an essential operation for efficiently iterating through pieces
    /// on a bitboard one at a time.
    ///
    /// # Returns
    ///
    /// * `Some(Square)` - The square corresponding to the removed least significant bit
    /// * `None` - If the bitboard is empty (no bits set)
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let mut multi_square = Bitboard::from(Square::E4) | Bitboard::from(Square::D2);
    /// assert_eq!(multi_square.pop_lsb(), Some(Square::D2)); // D2 has lower index than E4
    /// assert_eq!(multi_square.pop_lsb(), Some(Square::E4));
    /// assert_eq!(multi_square.pop_lsb(), None); // Now empty
    /// ```
    #[inline]
    pub fn pop_lsb(&mut self) -> Option<Square> {
        match self.0 {
            0 => None,
            bits => {
                let lsb_square = self.lsb().unwrap();
                self.0 = bits & (bits - 1); // Clear the LSB
                Some(lsb_square)
            }
        }
    }

    /// Returns the most significant bit (MSB) and removes it from the bitboard
    ///
    /// This allows you to process the "highest" pieces first.
    ///
    /// # Returns
    ///
    /// * `Some(Square)` - The square corresponding to the removed most significant bit
    /// * `None` - If the bitboard is empty (no bits set)
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let mut multi_square = Bitboard::from(Square::E4) | Bitboard::from(Square::G7);
    /// assert_eq!(multi_square.pop_msb(), Some(Square::G7)); // G7 has higher index than E4
    /// assert_eq!(multi_square.pop_msb(), Some(Square::E4));
    /// assert_eq!(multi_square.pop_msb(), None); // Now empty
    /// ```
    #[inline]
    pub fn pop_msb(&mut self) -> Option<Square> {
        match self.0 {
            0 => None,
            bits => {
                let msb_square = self.msb().unwrap();
                let msb_bit = 1u64 << (msb_square as u8 as u64);
                self.0 = bits & !msb_bit; // Clear the MSB
                Some(msb_square)
            }
        }
    }

    /// Iterates through all set bits in the bitboard, applying the given function to each square
    ///
    /// This function processes squares from least significant bit to most significant bit.
    /// It uses pop_lsb() internally to efficiently iterate through the bitboard.
    ///
    /// # Arguments
    ///
    /// * `f` - A function or closure that takes a Square as its parameter
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let mut squares = Vec::new();
    /// let bb = Bitboard::from(Square::E4) | Bitboard::from(Square::D5);
    ///
    /// bb.for_each(|square| {
    ///     squares.push(square);
    /// });
    ///
    /// assert_eq!(squares.len(), 2);
    /// assert!(squares.contains(&Square::E4));
    /// assert!(squares.contains(&Square::D5));
    /// ```
    #[inline]
    pub fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(Square),
    {
        let mut bb = *self;
        while let Some(square) = bb.pop_lsb() {
            f(square);
        }
    }

    /// Returns the number of bits set in the bitboard (population count)
    ///
    /// This is useful for counting the number of pieces or squares.
    ///
    /// # Returns
    ///
    /// The number of bits set to 1 in the bitboard
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let bb = Bitboard::from(Square::E4) | Bitboard::from(Square::D5);
    /// assert_eq!(bb.count_bits(), 2);
    ///
    /// let empty = Bitboard::EMPTY;
    /// assert_eq!(empty.count_bits(), 0);
    /// ```
    #[inline]
    pub fn count_bits(&self) -> u32 {
        self.0.count_ones()
    }

    /// Checks if the bitboard is empty (has no bits set)
    ///
    /// # Returns
    ///
    /// * `true` - If the bitboard has no bits set
    /// * `false` - If the bitboard has at least one bit set
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let bb = Bitboard::from(Square::E4);
    /// assert!(!bb.is_empty());
    ///
    /// let empty = Bitboard::EMPTY;
    /// assert!(empty.is_empty());
    /// ```
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    /// Checks if a specific square is set in the bitboard
    ///
    /// # Arguments
    ///
    /// * `square` - The square to check
    ///
    /// # Returns
    ///
    /// * `true` - If the bit corresponding to the square is set
    /// * `false` - If the bit corresponding to the square is not set
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let bb = Bitboard::from(Square::E4);
    /// assert!(bb.get(Square::E4));
    /// assert!(!bb.get(Square::A1));
    /// ```
    #[inline]
    pub fn get(&self, square: Square) -> bool {
        (self.0 & (1u64 << (square as u8 as u64))) != 0
    }

    /// Sets a specific square in the bitboard
    ///
    /// # Arguments
    ///
    /// * `square` - The square to set
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let mut bb = Bitboard::EMPTY;
    /// bb.set(Square::E4);
    /// assert!(bb.get(Square::E4));
    /// ```
    #[inline]
    pub fn set(&mut self, square: Square) {
        self.0 |= 1u64 << (square as u8 as u64);
    }

    /// Clears a specific square in the bitboard
    ///
    /// # Arguments
    ///
    /// * `square` - The square to clear
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let mut bb = Bitboard::from(Square::E4);
    /// bb.clear(Square::E4);
    /// assert!(!bb.get(Square::E4));
    /// ```
    #[inline]
    pub fn clear(&mut self, square: Square) {
        self.0 &= !(1u64 << (square as u8 as u64));
    }

    /// Toggles a specific square in the bitboard
    ///
    /// If the bit was set, it will be cleared.
    /// If the bit was clear, it will be set.
    ///
    /// # Arguments
    ///
    /// * `square` - The square to toggle
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use sophos::{Bitboard, Square};
    ///
    /// let mut bb = Bitboard::EMPTY;
    /// bb.toggle(Square::E4);
    /// assert!(bb.get(Square::E4));
    /// bb.toggle(Square::E4);
    /// assert!(!bb.get(Square::E4));
    /// ```
    #[inline]
    pub fn toggle(&mut self, square: Square) {
        self.0 ^= 1u64 << (square as u8 as u64);
    }

    /// Shifts the bitboard in the given direction, preventing edge wrapping.
    ///
    /// This function provides safe shifting operations that respect the chess board
    /// boundaries. This means bits that would wrap around the edge of the board are
    /// properly masked out before the shift operation.
    ///
    /// # Arguments
    /// * `dir` - The direction to shift the bitboard
    ///
    /// # Returns
    /// * `Bitboard` - The shifted bitboard. If the shift would move all bits off the board,
    ///                an empty bitboard is returned.
    pub fn shift(&self, dir: Direction) -> Bitboard {
        use Direction::*;

        let bb = *self;
        if bb.is_empty() {
            return Bitboard::EMPTY;
        }

        // Apply the appropriate mask and shift for each direction
        match dir {
            // North directions
            N => (bb & !Self::RANK_8) << 8,
            NN => (bb & !Self::RANK_78) << 16,
            NE => (bb & !Self::RANK_8 & !Self::FILE_H) << 9,
            NW => (bb & !Self::RANK_8 & !Self::FILE_A) << 7,
            NNE => (bb & !Self::RANK_78 & !Self::FILE_H) << 17,
            NNW => (bb & !Self::RANK_78 & !Self::FILE_A) << 15,
            NEE => (bb & !Self::RANK_8 & !Self::FILE_GH) << 10,
            NWW => (bb & !Self::RANK_8 & !Self::FILE_AB) << 6,

            // South directions
            S => (bb & !Self::RANK_1) >> 8,
            SS => (bb & !Self::RANK_12) >> 16,
            SE => (bb & !Self::RANK_1 & !Self::FILE_H) >> 7,
            SW => (bb & !Self::RANK_1 & !Self::FILE_A) >> 9,
            SSE => (bb & !Self::RANK_12 & !Self::FILE_H) >> 15,
            SSW => (bb & !Self::RANK_12 & !Self::FILE_A) >> 17,
            SEE => (bb & !Self::RANK_1 & !Self::FILE_GH) >> 6,
            SWW => (bb & !Self::RANK_1 & !Self::FILE_AB) >> 10,

            // Pure east/west
            E => (bb & !Self::FILE_H) << 1,
            W => (bb & !Self::FILE_A) >> 1,
        }
    }
}
/******************************************\
|==========================================|
|                 Display                  |
|==========================================|
\******************************************/

/// Display function for bitboards
impl fmt::Display for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const SEPARATOR: &str = "\n     +---+---+---+---+---+---+---+---+";

        writeln!(f, "{}", SEPARATOR)?;

        for rank in Rank::iter().rev() {
            write!(f, " {}   |", rank as u8 + 1)?;

            for file in File::iter() {
                let square = (file, rank).into();
                let cell = if self.get(square) { " 1 " } else { "   " };
                write!(f, "{}|", cell)?;
            }

            writeln!(f, "{}", SEPARATOR)?;
        }

        writeln!(f)?;
        writeln!(f, "       A   B   C   D   E   F   G   H")?;
        writeln!(f)?;
        writeln!(f, "Bitboard: {:#x}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lsb_msb() {
        // Test with single bits
        let a1 = Bitboard::from(Square::A1);
        assert_eq!(a1.lsb(), Some(Square::A1));
        assert_eq!(a1.msb(), Some(Square::A1));

        let h8 = Bitboard::from(Square::H8);
        assert_eq!(h8.lsb(), Some(Square::H8));
        assert_eq!(h8.msb(), Some(Square::H8));

        // Test with multiple bits
        let bb = Bitboard::from(Square::A1) | Bitboard::from(Square::H8);
        assert_eq!(bb.lsb(), Some(Square::A1)); // A1 has lower index than H8
        assert_eq!(bb.msb(), Some(Square::H8)); // H8 has higher index than A1

        // Test with empty bitboard
        let empty = Bitboard::EMPTY;
        assert_eq!(empty.lsb(), None);
        assert_eq!(empty.msb(), None);
    }

    #[test]
    fn test_pop_lsb() {
        // Test popping from a bitboard with multiple bits
        let mut bb = Bitboard::from(Square::E4) | Bitboard::from(Square::A1);
        assert_eq!(bb.pop_lsb(), Some(Square::A1)); // A1 has lowest index
        assert_eq!(bb.pop_lsb(), Some(Square::E4));
        assert_eq!(bb.pop_lsb(), None); // Now empty

        // Test popping from an already empty bitboard
        assert_eq!(bb.pop_lsb(), None);
    }

    #[test]
    fn test_pop_msb() {
        // Test popping from a bitboard with multiple bits
        let mut bb = Bitboard::from(Square::E4) | Bitboard::from(Square::H8);
        assert_eq!(bb.pop_msb(), Some(Square::H8)); // H8 has highest index
        assert_eq!(bb.pop_msb(), Some(Square::E4));
        assert_eq!(bb.pop_msb(), None); // Now empty
    }

    #[test]
    fn test_count_bits() {
        // Test empty bitboard
        let empty = Bitboard::EMPTY;
        assert_eq!(empty.count_bits(), 0);

        // Test single bit
        let single = Bitboard::from(Square::E4);
        assert_eq!(single.count_bits(), 1);

        // Test multiple bits
        let multi =
            Bitboard::from(Square::E4) | Bitboard::from(Square::D5) | Bitboard::from(Square::A1);
        assert_eq!(multi.count_bits(), 3);

        // Test all bits set (full board)
        let full = !Bitboard::EMPTY;
        assert_eq!(full.count_bits(), 64);
    }

    #[test]
    fn test_is_empty() {
        let empty = Bitboard::EMPTY;
        assert!(empty.is_empty());

        let non_empty = Bitboard::from(Square::E4);
        assert!(!non_empty.is_empty());
    }

    #[test]
    fn test_get_set_clear_toggle() {
        // Test set
        let mut bb = Bitboard::EMPTY;
        bb.set(Square::E4);
        assert!(bb.get(Square::E4));
        assert!(!bb.get(Square::A1));

        // Test clear
        bb.clear(Square::E4);
        assert!(!bb.get(Square::E4));

        // Test toggle
        bb.toggle(Square::D5);
        assert!(bb.get(Square::D5));
        bb.toggle(Square::D5);
        assert!(!bb.get(Square::D5));
    }

    #[test]
    fn test_bitloop_for_each() {
        let bb = Bitboard::from(Square::E4) | Bitboard::from(Square::D5);

        // Test bitloop
        let mut squares = Vec::new();
        bb.for_each(|sq| squares.push(sq));

        assert_eq!(squares.len(), 2);
        assert!(squares.contains(&Square::E4));
        assert!(squares.contains(&Square::D5));

        // Test for_each (should behave the same as bitloop)
        let mut squares2 = Vec::new();
        bb.for_each(|sq| squares2.push(sq));

        assert_eq!(squares2.len(), 2);
        assert!(squares2.contains(&Square::E4));
        assert!(squares2.contains(&Square::D5));
    }

    #[test]
    fn test_bitboard_operations() {
        // Test bitwise operations
        let a1 = Bitboard::from(Square::A1);
        let h8 = Bitboard::from(Square::H8);

        // OR operation
        let combined = a1 | h8;
        assert!(combined.get(Square::A1));
        assert!(combined.get(Square::H8));
        assert_eq!(combined.count_bits(), 2);

        // AND operation
        let intersection = a1 & h8;
        assert!(intersection.is_empty());

        // XOR operation
        let xor_result = a1 ^ a1;
        assert!(xor_result.is_empty());

        // NOT operation
        let inverted = !a1;
        assert!(!inverted.get(Square::A1));
        assert_eq!(inverted.count_bits(), 63);
    }

    #[test]
    fn test_shift_basic_directions() {
        let bb = Bitboard::from(Square::E5);

        // Cardinal directions
        assert_eq!(bb.shift(Direction::N), Bitboard::from(Square::E6));
        assert_eq!(bb.shift(Direction::S), Bitboard::from(Square::E4));
        assert_eq!(bb.shift(Direction::E), Bitboard::from(Square::F5));
        assert_eq!(bb.shift(Direction::W), Bitboard::from(Square::D5));

        // Diagonal directions
        assert_eq!(bb.shift(Direction::NE), Bitboard::from(Square::F6));
        assert_eq!(bb.shift(Direction::NW), Bitboard::from(Square::D6));
        assert_eq!(bb.shift(Direction::SE), Bitboard::from(Square::F4));
        assert_eq!(bb.shift(Direction::SW), Bitboard::from(Square::D4));

        // Double moves
        assert_eq!(bb.shift(Direction::NN), Bitboard::from(Square::E7));
        assert_eq!(bb.shift(Direction::SS), Bitboard::from(Square::E3));

        // Knight-like moves
        assert_eq!(bb.shift(Direction::NNE), Bitboard::from(Square::F7));
        assert_eq!(bb.shift(Direction::NNW), Bitboard::from(Square::D7));
        assert_eq!(bb.shift(Direction::NEE), Bitboard::from(Square::G6));
        assert_eq!(bb.shift(Direction::NWW), Bitboard::from(Square::C6));
        assert_eq!(bb.shift(Direction::SEE), Bitboard::from(Square::G4));
        assert_eq!(bb.shift(Direction::SWW), Bitboard::from(Square::C4));
        assert_eq!(bb.shift(Direction::SSE), Bitboard::from(Square::F3));
        assert_eq!(bb.shift(Direction::SSW), Bitboard::from(Square::D3));
    }

    #[test]
    fn test_shift_edge_cases() {
        // Test edge wrapping prevention

        // H-file (right edge) tests
        let h5 = Bitboard::from(Square::H5);
        assert_eq!(h5.shift(Direction::E), Bitboard::EMPTY);
        assert_eq!(h5.shift(Direction::NE), Bitboard::EMPTY);
        assert_eq!(h5.shift(Direction::SE), Bitboard::EMPTY);
        assert_eq!(h5.shift(Direction::W), Bitboard::from(Square::G5));

        // A-file (left edge) tests
        let a5 = Bitboard::from(Square::A5);
        assert_eq!(a5.shift(Direction::W), Bitboard::EMPTY);
        assert_eq!(a5.shift(Direction::NW), Bitboard::EMPTY);
        assert_eq!(a5.shift(Direction::SW), Bitboard::EMPTY);
        assert_eq!(a5.shift(Direction::E), Bitboard::from(Square::B5));

        // Rank 8 (top edge) tests
        let e8 = Bitboard::from(Square::E8);
        assert_eq!(e8.shift(Direction::N), Bitboard::EMPTY);
        assert_eq!(e8.shift(Direction::NE), Bitboard::EMPTY);
        assert_eq!(e8.shift(Direction::NW), Bitboard::EMPTY);
        assert_eq!(e8.shift(Direction::S), Bitboard::from(Square::E7));

        // Rank 1 (bottom edge) tests
        let e1 = Bitboard::from(Square::E1);
        assert_eq!(e1.shift(Direction::S), Bitboard::EMPTY);
        assert_eq!(e1.shift(Direction::SE), Bitboard::EMPTY);
        assert_eq!(e1.shift(Direction::SW), Bitboard::EMPTY);
        assert_eq!(e1.shift(Direction::N), Bitboard::from(Square::E2));

        // G-file edge cases for double-east shifts
        let g5 = Bitboard::from(Square::G5);
        assert_eq!(g5.shift(Direction::NEE), Bitboard::EMPTY);
        assert_eq!(g5.shift(Direction::SEE), Bitboard::EMPTY);

        // B-file edge cases for double-west shifts
        let b5 = Bitboard::from(Square::B5);
        assert_eq!(b5.shift(Direction::NWW), Bitboard::EMPTY);
        assert_eq!(b5.shift(Direction::SWW), Bitboard::EMPTY);
    }

    #[test]
    fn test_shift_multiple_bits() {
        // Test with multiple bits set
        let bb = Bitboard::from(Square::E4) | Bitboard::from(Square::D4);

        // Shift north
        assert_eq!(
            bb.shift(Direction::N),
            Bitboard::from(Square::E5) | Bitboard::from(Square::D5)
        );

        // Shift east
        assert_eq!(
            bb.shift(Direction::E),
            Bitboard::from(Square::F4) | Bitboard::from(Square::E4)
        );

        // Test with edge bits that would wrap
        let edge_case = Bitboard::from(Square::H1) | Bitboard::from(Square::A1);

        // East shift should only move the A1 bit to B1
        assert_eq!(edge_case.shift(Direction::E), Bitboard::from(Square::B1));

        // West shift should only move the H1 bit to G1
        assert_eq!(edge_case.shift(Direction::W), Bitboard::from(Square::G1));
    }
}
