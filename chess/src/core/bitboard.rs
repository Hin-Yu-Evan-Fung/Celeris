use std::fmt;

use super::{Colour, Direction, File, PieceType, Rank, Square};

/******************************************\
|==========================================|
|                 Bitboard                 |
|==========================================|
\******************************************/

/// Represents a 64-bit bitboard, commonly used in chess engines
/// to represent sets of squares on the chessboard.
/// Each bit corresponds to a square, from A1 (LSB) to H8 (MSB).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Bitboard(pub u64);

crate::impl_bit_ops!(Bitboard);
crate::impl_bit_mani_ops!(Bitboard, u8);

/******************************************\
|==========================================|
|           Bitboard Constants             |
|==========================================|
\******************************************/

impl Bitboard {
    /// An empty bitboard, with no bits set.
    pub const EMPTY: Bitboard = Bitboard(0);

    /// A full bitboard, with all 64 bits set.
    pub const FULL: Bitboard = Bitboard(!Self::EMPTY.0);

    /// A bitboard with only the A1 square set.
    pub const A1: Bitboard = Bitboard(1);

    /// A bitboard representing all squares on the 1st rank.
    pub const RANK_1: Bitboard = Bitboard(0x00000000000000ff);

    /// A bitboard representing all squares on the 8th rank.
    pub const RANK_8: Bitboard = Bitboard(0xff00000000000000);

    /// A bitboard representing all squares on the 1st and 2nd ranks.
    pub const RANK_12: Bitboard = Bitboard(0x000000000000ffff);

    /// A bitboard representing all squares on the 7th and 8th ranks.
    pub const RANK_78: Bitboard = Bitboard(0xffff000000000000);

    /// A bitboard representing all squares on the A file.
    pub const FILE_A: Bitboard = Bitboard(0x0101010101010101);

    /// A bitboard representing all squares on the H file.
    pub const FILE_H: Bitboard = Bitboard(0x8080808080808080);

    /// A bitboard representing all squares on the A and B file.
    const FILE_AB: Bitboard = Bitboard(0x303030303030303);

    /// A bitboard representing all squares on the G and H file.
    const FILE_GH: Bitboard = Bitboard(0xC0C0C0C0C0C0C0C0);
}

/******************************************\
|==========================================|
|                Conversions               |
|==========================================|
\******************************************/

impl Square {
    /// Converts a `Square` into a `Bitboard` with only that square's bit set.
    pub const fn bb(&self) -> Bitboard {
        Bitboard(Bitboard::A1.0 << *self as u8)
    }
}

impl Rank {
    /// Converts a `Rank` into a `Bitboard` with all squares on that rank set.
    pub const fn bb(&self) -> Bitboard {
        Bitboard(Bitboard::RANK_1.0 << (8 * *self as u8))
    }
}

impl File {
    /// Converts a `File` into a `Bitboard` with all squares on that file set.
    pub const fn bb(&self) -> Bitboard {
        Bitboard(Bitboard::FILE_A.0 << *self as u8)
    }
}

impl<const N: usize> From<[Square; N]> for Bitboard {
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
|        Bitboard Implementation         |
|==========================================|
\******************************************/

impl Bitboard {
    /// Finds the least significant bit (LSB) set in the bitboard and returns its corresponding `Square`.
    /// Returns `None` if the bitboard is empty.
    #[inline]
    /// ## Examples
    /// ```rust
    /// use chess::core::{Square, Bitboard};
    /// assert_eq!((Square::A1.bb() | Square::H8.bb()).lsb(), Some(Square::A1));
    /// assert_eq!(Bitboard::EMPTY.lsb(), None);
    /// ```
    pub const fn lsb(&self) -> Option<Square> {
        match self.0 {
            0 => None,
            bits => unsafe { Some(Square::from_unchecked(bits.trailing_zeros() as u8)) },
        }
    }

    /// Finds the least significant bit (LSB) set in the bitboard and returns its corresponding `Square`.
    ///
    /// # Panics
    /// Panics in debug mode if the bitboard is empty.
    ///
    /// # Safety
    /// This function is unsafe because it assumes the bitboard is not empty. Calling it on an empty
    /// bitboard will result in undefined behavior (though `trailing_zeros` on 0 is well-defined as 64).
    /// The primary unsafety comes from `Square::from_unchecked` if the result of `trailing_zeros` is not a valid square index.
    pub const fn lsb_unchecked(&self) -> Square {
        debug_assert!(self.0 != 0, "Bitboard is empty");
        unsafe { Square::from_unchecked(self.0.trailing_zeros() as u8) }
    }

    /// Finds and removes (clears) the least significant bit (LSB) from the bitboard,
    /// returning its corresponding `Square`. Returns `None` if the bitboard was empty.
    #[inline]
    pub const fn pop_lsb(&mut self) -> Option<Square> {
        match self.0 {
            0 => None,
            _ => {
                let lsb_square = self.lsb_unchecked();
                self.0 &= self.0 - 1;
                Some(lsb_square)
            }
        }
    }

    /// Finds and removes (clears) the least significant bit (LSB) from the bitboard,
    /// returning its corresponding `Square`.
    ///
    /// # Panics
    /// Panics in debug mode if the bitboard is empty.
    /// # Safety
    /// Assumes the bitboard is not empty. See `lsb_unchecked` for safety notes.
    #[inline]
    pub const fn pop_lsb_unchecked(&mut self) -> Square {
        debug_assert!(self.0 != 0, "Bitboard is empty");
        let lsb_square = self.lsb_unchecked();
        self.0 &= self.0 - 1;
        lsb_square
    }

    /// Counts the number of set bits (population count) in the bitboard.
    #[inline]
    pub const fn count_bits(&self) -> u32 {
        self.0.count_ones()
    }

    /// Checks if the bitboard is empty (no bits set).
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.0 == 0
    }

    /// Checks if the bitboard has at least one bit set.
    #[inline]
    pub const fn is_occupied(&self) -> bool {
        self.0 != 0
    }

    /// Checks if the bit corresponding to the given `Square` is set.
    #[inline]
    pub const fn contains(&self, square: Square) -> bool {
        (self.0 & (1u64 << (square as u8 as u64))) != 0
    }

    /// Sets the bit corresponding to the given `Square`.
    #[inline]
    pub const fn set(&mut self, square: Square) {
        self.0 |= 1u64 << (square as u8 as u64);
    }

    /// Clears the bit corresponding to the given `Square`.
    #[inline]
    pub const fn clear(&mut self, square: Square) {
        self.0 &= !(1u64 << (square as u8 as u64));
    }

    /// Toggles the bit corresponding to the given `Square`.
    #[inline]
    pub const fn toggle(&mut self, square: Square) {
        self.0 ^= 1u64 << (square as u8 as u64);
    }

    /// Iterates over each set bit in the bitboard, calling the provided function `f` with the `Square` for each.
    #[inline]
    pub fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(Square),
    {
        let mut bb = *self;
        while bb.0 != 0 {
            f(bb.pop_lsb_unchecked());
        }
    }

    /// Checks if exactly one bit is set in the bitboard.
    #[inline]
    pub const fn is_singleton(&self) -> bool {
        !self.is_empty() && !self.more_than_one()
    }

    /// Checks if more than one bit is set in the bitboard.
    #[inline]
    pub const fn more_than_one(&self) -> bool {
        self.0 & (self.0.wrapping_sub(1)) != 0
    }

    /// Extracts bits from the bitboard according to a mask using the PEXT instruction.
    /// This instruction is available on processors with BMI2 support.
    #[cfg(target_feature = "bmi2")]
    #[inline]
    pub fn pext(&self, mask: u64) -> u64 {
        use core::arch::x86_64::_pext_u64;
        unsafe { _pext_u64(self.0, mask) }
    }
    // }

    // /******************************************\
    // |==========================================|
    // |              Board Helpers               |
    // |==========================================|
    // \******************************************/
    // impl Bitboard {
    /// Returns a bitboard of all ranks "in front" of the given square from the perspective of `col`.
    #[inline]
    pub fn forward_ranks(col: Colour, sq: Square) -> Bitboard {
        match col {
            Colour::White => !Self::RANK_1 << 8 * (sq.relative(col).rank() as u8),
            Colour::Black => !Self::RANK_8 >> 8 * (sq.relative(col).rank() as u8),
        }
    }

    /// Returns a bitboard of all squares on the same file as `sq` and "in front" of `sq` from `col`'s perspective.
    #[inline]
    pub fn forward_file(col: Colour, sq: Square) -> Bitboard {
        Self::forward_ranks(col, sq) & sq.file().bb()
    }

    /// Returns a bitboard of files adjacent to the file of `sq`.
    #[inline]
    pub fn adjacent_files(sq: Square) -> Bitboard {
        let bb = sq.file().bb();
        bb.shift(Direction::E) | bb.shift(Direction::W)
    }

    /// Returns the pawn attack span for a pawn of `col` on `sq`. This includes squares diagonally forward.
    #[inline]
    pub fn pawn_attack_span(col: Colour, sq: Square) -> Bitboard {
        Self::forward_ranks(col, sq) & Self::adjacent_files(sq)
    }

    /// Returns the passed pawn span for a pawn of `col` on `sq`. This includes the forward file and attack span.
    #[inline]
    pub fn passed_pawn_span(col: Colour, sq: Square) -> Bitboard {
        Self::forward_file(col, sq) | Self::pawn_attack_span(col, sq)
    }

    #[inline]
    pub fn pawn_attacks(col: Colour, bb: Bitboard) -> Bitboard {
        match col {
            Colour::White => bb.shift(Direction::NE) | bb.shift(Direction::NW),
            Colour::Black => bb.shift(Direction::SE) | bb.shift(Direction::SW),
        }
    }

    /// Rotates the board to the left/right based on the values sign, +ve means rotate left, -ve means rotate right
    #[inline]
    const fn rotate_left(&self, shift: i16) -> Bitboard {
        let bb = if shift >= 0 {
            self.0.rotate_left(shift as u32)
        } else {
            self.0.rotate_right(-shift as u32)
        };
        Bitboard(bb)
    }

    /// Shifts all set bits in the bitboard by one step in the given `Direction`. Bits shifted off the board are lost.
    #[inline]
    pub(crate) const fn shift(&self, dir: Direction) -> Bitboard {
        let bb = *self;

        Bitboard(bb.0 & Self::avoid_wrap(dir).0).rotate_left(dir as i16)
    }

    /// Returns a mask to prevent wrap-around when shifting in a given `Direction`.
    const fn avoid_wrap(dir: Direction) -> Bitboard {
        use Direction::*;
        let bb = match dir {
            SSE => Self::RANK_12.0 | Self::FILE_H.0,
            SEE => Self::RANK_1.0 | Self::FILE_GH.0,
            SWW => Self::RANK_1.0 | Self::FILE_AB.0,
            SSW => Self::RANK_12.0 | Self::FILE_A.0,
            NNW => Self::RANK_78.0 | Self::FILE_A.0,
            NNE => Self::RANK_78.0 | Self::FILE_H.0,
            NWW => Self::RANK_8.0 | Self::FILE_AB.0,
            NEE => Self::RANK_8.0 | Self::FILE_GH.0,

            N => Self::RANK_8.0,
            S => Self::RANK_1.0,
            E => Self::FILE_H.0,
            W => Self::FILE_A.0,

            NE => Self::RANK_8.0 | Self::FILE_H.0,
            NW => Self::RANK_8.0 | Self::FILE_A.0,
            SE => Self::RANK_1.0 | Self::FILE_H.0,
            SW => Self::RANK_1.0 | Self::FILE_A.0,

            NN => Self::RANK_78.0,
            SS => Self::RANK_12.0,
        };
        Bitboard(!bb)
    }

    /// Generates a bitboard of all squares attacked by a sliding piece in a given `Direction`, considering occluding pieces.
    /// Uses the [Kogge-Stone Algorithm](https://www.chessprogramming.org/Kogge-Stone_Algorithm)
    const fn occluded_fill(self, mut empty: Bitboard, dir: Direction) -> Bitboard {
        let shift = dir as i16;
        empty.0 &= Self::avoid_wrap(dir).0;
        let mut bb = Bitboard(self.0 & Self::avoid_wrap(dir).0);
        bb.0 |= empty.0 & bb.rotate_left(shift).0;
        empty.0 &= empty.rotate_left(shift).0;
        bb.0 |= empty.0 & bb.rotate_left(2 * shift).0;
        empty.0 &= empty.rotate_left(2 * shift).0;
        bb.0 |= empty.0 & bb.rotate_left(4 * shift).0;
        bb
    }

    /// Calculates sliding attacks from squares in `bb` in a specific `dir`, blocked by `occ`.
    const fn sliding_attack(bb: Bitboard, occ: Bitboard, dir: Direction) -> Bitboard {
        bb.occluded_fill(Bitboard(!occ.0), dir).shift(dir)
    }

    /// Calculates attacks for a bishop or rook from squares in `bb`, blocked by `occ`.
    pub(crate) const fn attack_on_the_fly(pt: PieceType, bb: Bitboard, occ: Bitboard) -> Bitboard {
        let bb = match pt {
            PieceType::Bishop => {
                Bitboard::sliding_attack(bb, occ, Direction::NE).0
                    | Bitboard::sliding_attack(bb, occ, Direction::NW).0
                    | Bitboard::sliding_attack(bb, occ, Direction::SE).0
                    | Bitboard::sliding_attack(bb, occ, Direction::SW).0
            }
            PieceType::Rook => {
                Bitboard::sliding_attack(bb, occ, Direction::N).0
                    | Bitboard::sliding_attack(bb, occ, Direction::S).0
                    | Bitboard::sliding_attack(bb, occ, Direction::E).0
                    | Bitboard::sliding_attack(bb, occ, Direction::W).0
            }
            _ => unreachable!(),
        };
        Bitboard(bb)
    }

    // Pawn specific ranks
    /// Returns the bitboard representing the typical starting rank for pawns of `col` (2nd for white, 7th for black).
    #[inline]
    pub const fn push_rank(col: Colour) -> Bitboard {
        match col {
            Colour::White => Rank::Rank2.bb(),
            Colour::Black => Rank::Rank7.bb(),
        }
    }

    /// Returns the bitboard representing the promotion rank for pawns of `col` (7th for white, 2nd for black).
    #[inline]
    pub const fn promo_rank(col: Colour) -> Bitboard {
        match col {
            Colour::White => Rank::Rank7.bb(),
            Colour::Black => Rank::Rank2.bb(),
        }
    }

    /// Returns the bitboard representing the rank where an en passant capture might occur for `col`. (3rd for white, 6th for black).
    #[inline]
    pub const fn ep_rank(col: Colour) -> Bitboard {
        match col {
            Colour::White => Rank::Rank3.bb(),
            Colour::Black => Rank::Rank6.bb(),
        }
    }
}

/******************************************\
|==========================================|
|                 Display                  |
|==========================================|
\******************************************/

impl fmt::Display for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const SEPARATOR: &str = "\n     +---+---+---+---+---+---+---+---+";

        writeln!(f, "{}", SEPARATOR)?;

        for rank in Rank::iter().rev() {
            write!(f, " {}   |", rank as u8 + 1)?;

            for file in File::iter() {
                let square = Square::from_parts(file, rank);
                let cell = if self.contains(square) { " 1 " } else { "   " };
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

/******************************************\
|==========================================|
|                Unit Tests                |
|==========================================|
\******************************************/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lsb_msb() {
        let a1 = Square::A1.bb();
        assert_eq!(a1.lsb(), Some(Square::A1));

        let h8 = Square::H8.bb();
        assert_eq!(h8.lsb(), Some(Square::H8));
    }

    #[test]
    fn test_pop_lsb() {
        let mut bb = Square::E4.bb() | Square::A1.bb();
        assert_eq!(bb.pop_lsb(), Some(Square::A1));
        assert_eq!(bb.pop_lsb(), Some(Square::E4));
        assert_eq!(bb.pop_lsb(), None);

        assert_eq!(bb.pop_lsb(), None);
    }

    #[test]
    fn test_count_bits() {
        let empty = Bitboard::EMPTY;
        assert_eq!(empty.count_bits(), 0);

        let single = Square::E4.bb();
        assert_eq!(single.count_bits(), 1);

        let multi = Square::E4.bb() | Square::D5.bb() | Square::A1.bb();
        assert_eq!(multi.count_bits(), 3);

        let full = !Bitboard::EMPTY;
        assert_eq!(full.count_bits(), 64);
    }

    #[test]
    fn test_is_empty() {
        let empty = Bitboard::EMPTY;
        assert!(empty.is_empty());

        let non_empty = Square::E4.bb();
        assert!(!non_empty.is_empty());
    }

    #[test]
    fn test_get_set_clear_toggle() {
        let mut bb = Bitboard::EMPTY;
        bb.set(Square::E4);
        assert!(bb.contains(Square::E4));
        assert!(!bb.contains(Square::A1));

        bb.clear(Square::E4);
        assert!(!bb.contains(Square::E4));

        bb.toggle(Square::D5);
        assert!(bb.contains(Square::D5));
        bb.toggle(Square::D5);
        assert!(!bb.contains(Square::D5));
    }

    #[test]
    fn test_bitloop_for_each() {
        let bb = Square::E4.bb() | Square::D5.bb();

        let mut squares = Vec::new();
        bb.for_each(|sq| squares.push(sq));

        assert_eq!(squares.len(), 2);
        assert!(squares.contains(&Square::E4));
        assert!(squares.contains(&Square::D5));

        let mut squares2 = Vec::new();
        bb.for_each(|sq| squares2.push(sq));

        assert_eq!(squares2.len(), 2);
        assert!(squares2.contains(&Square::E4));
        assert!(squares2.contains(&Square::D5));
    }

    #[test]
    fn test_bitboard_operations() {
        let a1 = Square::A1.bb();
        let h8 = Square::H8.bb();

        let combined = a1 | h8;
        assert!(combined.contains(Square::A1));
        assert!(combined.contains(Square::H8));
        assert_eq!(combined.count_bits(), 2);

        let intersection = a1 & h8;
        assert!(intersection.is_empty());

        let xor_result = a1 ^ a1;
        assert!(xor_result.is_empty());

        let inverted = !a1;
        assert!(!inverted.contains(Square::A1));
        assert_eq!(inverted.count_bits(), 63);
    }

    #[test]
    fn test_shift_basic_directions() {
        let bb = Square::E5.bb();

        assert_eq!(bb.shift(Direction::N), Square::E6.bb());
        assert_eq!(bb.shift(Direction::S), Square::E4.bb());
        assert_eq!(bb.shift(Direction::E), Square::F5.bb());
        assert_eq!(bb.shift(Direction::W), Square::D5.bb());

        assert_eq!(bb.shift(Direction::NE), Square::F6.bb());
        assert_eq!(bb.shift(Direction::NW), Square::D6.bb());
        assert_eq!(bb.shift(Direction::SE), Square::F4.bb());
        assert_eq!(bb.shift(Direction::SW), Square::D4.bb());

        assert_eq!(bb.shift(Direction::NN), Square::E7.bb());
        assert_eq!(bb.shift(Direction::SS), Square::E3.bb());

        assert_eq!(bb.shift(Direction::NNE), Square::F7.bb());
        assert_eq!(bb.shift(Direction::NNW), Square::D7.bb());
        assert_eq!(bb.shift(Direction::NEE), Square::G6.bb());
        assert_eq!(bb.shift(Direction::NWW), Square::C6.bb());
        assert_eq!(bb.shift(Direction::SEE), Square::G4.bb());
        assert_eq!(bb.shift(Direction::SWW), Square::C4.bb());
        assert_eq!(bb.shift(Direction::SSE), Square::F3.bb());
        assert_eq!(bb.shift(Direction::SSW), Square::D3.bb());
    }

    #[test]
    fn test_shift_edge_cases() {
        let h5 = Square::H5.bb();
        assert_eq!(h5.shift(Direction::E), Bitboard::EMPTY);
        assert_eq!(h5.shift(Direction::NE), Bitboard::EMPTY);
        assert_eq!(h5.shift(Direction::SE), Bitboard::EMPTY);
        assert_eq!(h5.shift(Direction::W), Square::G5.bb());

        let a5 = Square::A5.bb();
        assert_eq!(a5.shift(Direction::W), Bitboard::EMPTY);
        assert_eq!(a5.shift(Direction::NW), Bitboard::EMPTY);
        assert_eq!(a5.shift(Direction::SW), Bitboard::EMPTY);
        assert_eq!(a5.shift(Direction::E), Square::B5.bb());

        let e8 = Square::E8.bb();
        assert_eq!(e8.shift(Direction::N), Bitboard::EMPTY);
        assert_eq!(e8.shift(Direction::NE), Bitboard::EMPTY);
        assert_eq!(e8.shift(Direction::NW), Bitboard::EMPTY);
        assert_eq!(e8.shift(Direction::S), Square::E7.bb());

        let e1 = Square::E1.bb();
        assert_eq!(e1.shift(Direction::S), Bitboard::EMPTY);
        assert_eq!(e1.shift(Direction::SE), Bitboard::EMPTY);
        assert_eq!(e1.shift(Direction::SW), Bitboard::EMPTY);
        assert_eq!(e1.shift(Direction::N), Square::E2.bb());

        let g5 = Square::G5.bb();
        assert_eq!(g5.shift(Direction::NEE), Bitboard::EMPTY);
        assert_eq!(g5.shift(Direction::SEE), Bitboard::EMPTY);

        let b5 = Square::B5.bb();
        assert_eq!(b5.shift(Direction::NWW), Bitboard::EMPTY);
        assert_eq!(b5.shift(Direction::SWW), Bitboard::EMPTY);
    }

    #[test]
    fn test_shift_multiple_bits() {
        let bb = Square::E4.bb() | Square::D4.bb();

        assert_eq!(bb.shift(Direction::N), Square::E5.bb() | Square::D5.bb());

        assert_eq!(bb.shift(Direction::E), Square::F4.bb() | Square::E4.bb());

        let edge_case = Square::H1.bb() | Square::A1.bb();

        assert_eq!(edge_case.shift(Direction::E), Square::B1.bb());

        assert_eq!(edge_case.shift(Direction::W), Square::G1.bb());
    }
}
