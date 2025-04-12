use super::magic_numbers::{BISHOP_MAGICS, ROOK_MAGICS};
use crate::core::*;

/******************************************\
|==========================================|
|             Magics Definition            |
|==========================================|
\******************************************/

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Magic {
    pub(crate) magic: u64,
    pub(crate) mask: Bitboard,
    pub(crate) shift: u8,
    pub(crate) offset: usize,
}

impl Magic {
    pub(crate) fn index(self, occ: Bitboard) -> usize {
        (((occ & self.mask).0.wrapping_mul(self.magic)) >> self.shift) as usize + self.offset
    }
}

/******************************************\
|==========================================|
|              Attack Tables               |
|==========================================|
\******************************************/

/// # Attack Table
///
/// ## Elements
/// - table - Hash table for bishop/rook moves
/// - magic - Contains information about table key generation
///
/// ## Function
/// - get_entry(sq, occ) - Gets the corresponding attack pattern for a given square and occupancy
pub(crate) struct SliderAttackTable<const N: usize> {
    pub(crate) table: Box<[Bitboard; N]>,
    pub(crate) magic: [Magic; Square::NUM],
}

///
impl<const N: usize> SliderAttackTable<N> {
    /// ### Gets the corresponding attack pattern for a given square and occupancy
    pub(crate) fn get_entry(&self, sq: Square, occ: Bitboard) -> Bitboard {
        let magic = self.magic[sq as usize];
        let index = magic.index(occ);
        self.table[index as usize]
    }
}

/******************************************\
|==========================================|
|            Attacks on the fly            |
|==========================================|
\******************************************/

/// # Attacks on the fly
/// - Calculate the attacks of a piece on the fly (Slow approach used to populate
/// tables).
/// - This is used for rook and bishop attacks.
pub(crate) fn attacks_on_the_fly(pt: PieceType, sq: Square, occ: Bitboard) -> Bitboard {
    use Direction::*;
    // Directions for rook and bishop
    let dirs: [Direction; 4] = match pt {
        PieceType::Rook => [N, E, W, S],
        PieceType::Bishop => [NE, NW, SE, SW],
        _ => unreachable!(),
    };

    let mut attacks = Bitboard::EMPTY;
    // Loop through the directions for the piece type
    for dir in dirs {
        let mut to = sq;
        // Shift in the direction if the current square is empty and not occupied
        while !occ.get(to) {
            to = match to + dir {
                Ok(to) => to,
                Err(_) => break,
            };
            attacks |= to.into();
        }
        // The last square is either occupied or at the border
    }
    attacks
}

/******************************************\
|==========================================|
|           Finding Magic Numbers          |
|==========================================|
\******************************************/

/// # Get Magic Tables (If you have a list of magics)
/// - This function is used to generate the magic and attack table using existing magic numbers
pub(super) fn get_magic_tables<const N: usize>(pt: PieceType) -> SliderAttackTable<N> {
    let mut offset = 0;
    let mut table = Box::new([Bitboard::EMPTY; N]);
    let mut magic = [Magic::default(); Square::NUM];

    let magic_numbers = match pt {
        PieceType::Bishop => BISHOP_MAGICS,
        PieceType::Rook => ROOK_MAGICS,
        _ => unreachable!(),
    };

    // Loop through the squares and generate the magic numbers for the piece type
    for sq in Square::iter() {
        let mut m = init_magic_struct(pt, sq, &mut offset);

        m.magic = magic_numbers[sq as usize];

        let perm = 1 << m.mask.count_bits();

        // Populate the reference and occupancy tables for the piece type on the square
        let mut occ = Bitboard::EMPTY;
        for _ in 0..perm {
            // Generate the corresponding attack rays for the occupancy, piece type, and square
            table[m.index(occ)] = attacks_on_the_fly(pt, sq, occ);
            // Save occupancy bitboard at the current index
            occ = (occ - m.mask) & m.mask;
        }

        magic[sq as usize] = m;
    }

    // Return the attack tables for the piece type
    SliderAttackTable { table, magic }
}

/******************************************\
|==========================================|
|            Helper Functions              |
|==========================================|
\******************************************/

/// # Edge Mask
/// - The edge mask is used to remove the edges of the board from the attack mask.
/// - The edges of the board are not included in the attack lookup because it doesn't matter if there is a blocker there or not.
/// - Except in the cases where the attacking piece is on the edge, if so the edge mask should not include the attack area.
pub(crate) fn get_edge_mask(sq: Square) -> Bitboard {
    use File::*;
    use Rank::*;

    let rank_18bb: Bitboard = Bitboard::from(Rank1) | Bitboard::from(Rank8);
    let file_ahbb: Bitboard = Bitboard::from(FileA) | Bitboard::from(FileH);

    let sq_rank_bb = Bitboard::from(sq.rank());
    let sq_file_bb = Bitboard::from(sq.file());

    // Get the edges of the board
    (rank_18bb & !sq_rank_bb) | (file_ahbb & !sq_file_bb)
}

pub(crate) fn init_magic_struct(pt: PieceType, sq: Square, offset: &mut usize) -> Magic {
    let mut m = Magic::default();
    m.magic = 0;
    m.mask = attacks_on_the_fly(pt, sq, Bitboard::EMPTY) & !get_edge_mask(sq);
    m.shift = 64 - m.mask.count_bits() as u8;
    m.offset = *offset;
    *offset += 1 << m.mask.count_bits();

    m
}
