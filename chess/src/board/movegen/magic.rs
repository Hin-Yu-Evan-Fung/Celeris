use crate::core::*;
use std::sync::LazyLock;

/******************************************\
|==========================================|
|             Magics Definition            |
|==========================================|
\******************************************/

#[derive(Debug, Default, Clone, Copy)]
pub struct Magic {
    #[cfg(not(target_feature = "bmi2"))]
    magic: u64,

    mask: Bitboard,
    #[cfg(not(target_feature = "bmi2"))]
    shift: u8,

    offset: usize,
}

impl Magic {
    const EMPTY: Magic = Magic {
        #[cfg(not(target_feature = "bmi2"))]
        magic: 0,
        mask: Bitboard::EMPTY,
        #[cfg(not(target_feature = "bmi2"))]
        shift: 0,
        offset: 0,
    };

    #[cfg(not(target_feature = "bmi2"))]
    #[inline]
    pub(crate) const fn index(self, occ: Bitboard) -> usize {
        ((occ.0 & self.mask.0).wrapping_mul(self.magic)).wrapping_shr(self.shift as u32) as usize
            + self.offset
    }

    #[cfg(target_feature = "bmi2")]
    pub(crate) fn index(self, occ: Bitboard) -> usize {
        occ.pext(self.mask.0) as usize + self.offset
    }
}

pub type MagicTable = [Magic; Square::NUM];

pub const BISHOP_MAGICS: MagicTable = populate_magic_table(PieceType::Bishop);

pub const ROOK_MAGICS: MagicTable = populate_magic_table(PieceType::Rook);

const BISHOP_TABLE_SIZE: usize = 0x1480;

const ROOK_TABLE_SIZE: usize = 0x19000;

pub static BISHOP_TABLE: LazyLock<Box<[Bitboard; 0x1480]>> = LazyLock::new(|| {
    populate_attack_table::<BISHOP_TABLE_SIZE>(PieceType::Bishop)
        .into_boxed_slice()
        .try_into()
        .expect("Failed to compile bishop table")
});

pub static ROOK_TABLE: LazyLock<Box<[Bitboard; 0x19000]>> = LazyLock::new(|| {
    populate_attack_table::<ROOK_TABLE_SIZE>(PieceType::Rook)
        .into_boxed_slice()
        .try_into()
        .expect("Failed to compile rook table")
});

pub fn init_magic_tables() {
    let _ = &*BISHOP_TABLE;
    let _ = &*ROOK_TABLE;
}

/******************************************\
|==========================================|
|               Magic Numbers              |
|==========================================|
\******************************************/

#[cfg(not(target_feature = "bmi2"))]
pub(super) const BISHOP_MAGIC_NUMS: [u64; 64] = [
    0x1200440A0890200,
    0x2040122021A0407,
    0x4008880108210401,
    0x211040080020000,
    0xA8A2121004000005,
    0xA8A2121004000005,
    0x4008880108210401,
    0x8000804822012000,
    0x888008085094004C,
    0x888008085094004C,
    0x8000080214002010,
    0x2204880A02200001,
    0xC1840420000200,
    0x408220601223,
    0x4424042C0CA0,
    0x4424042C0CA0,
    0x2408410900200D8,
    0x1004001010220048,
    0x88088400240010,
    0x2832400401020008,
    0x80A4000494201400,
    0x2400808021080,
    0x8000A00202112000,
    0x2044300822100,
    0x2050A405845800,
    0x200824C0083000A9,
    0x500440208080014,
    0x8144800008200C0,
    0x1421010001104001,
    0x430008403004500,
    0xA84042231010102,
    0xA84042231010102,
    0x2202000501301,
    0x4144200041000,
    0x8002002E03100080,
    0x340600800010810,
    0x6020020020040408,
    0x65004A120020205,
    0x8081040080043200,
    0x8081040080043200,
    0x8001042120200420,
    0x8001042120200420,
    0x10C0402002C00,
    0x4202024200801,
    0x4029101200921200,
    0x4002224045004600,
    0xC0900128010010A8,
    0x4002224045004600,
    0x2082280104111100,
    0x28440421080044,
    0x280004404040101,
    0x2100084110008,
    0x802001222020C80,
    0x400204010A12,
    0x8204102020000,
    0x20040440404001,
    0x130A08404202A51,
    0x428400888400,
    0x4312022111000,
    0x810008040A841100,
    0x4410010810020880,
    0x862042002020208,
    0x862042002020208,
    0x82100116240041,
];

#[cfg(not(target_feature = "bmi2"))]
pub(super) const ROOK_MAGIC_NUMS: [u64; 64] = [
    0x80002018804000,
    0xA040004010002000,
    0x100200040081100,
    0x4100100021000408,
    0x1100100800040300,
    0x1100082100440082,
    0x6580230000800200,
    0x100010008812052,
    0x18800428400088,
    0x42004102002088,
    0xA208801000200084,
    0x425000A21001000,
    0x402000408102200,
    0x2000402001008,
    0x4E46000102002804,
    0x62000208941049,
    0x2000848008400020,
    0x3100404000201000,
    0x440808020001000,
    0x14120008402200,
    0x2002020004100820,
    0x216808002008400,
    0x4484040001080290,
    0x8400020000804401,
    0x4920208180014002,
    0xA00500040002000,
    0xC800110100200840,
    0x2000084200120020,
    0x8000404002040,
    0x4914020080040080,
    0x400010400081002,
    0x9100010025C982,
    0x5709604005800081,
    0x410002000400048,
    0x200080801000,
    0x2200801000800802,
    0x1001005000800,
    0x44008004800200,
    0x1840512804004210,
    0x4C0220449200010C,
    0x1000824015608000,
    0x500804001130020,
    0x20080010004040,
    0x224100009010020,
    0x82000804220011,
    0x406000804010100,
    0x2200018802040030,
    0x80C100008861000A,
    0x1008040220200,
    0x1008040220200,
    0x8040200010008080,
    0x4000800800100080,
    0xA000080080040080,
    0x80040002008080,
    0xC1080110020400,
    0x200010844028600,
    0x281002080004019,
    0x100104004248101,
    0x500204200081082,
    0x802080520100101,
    0x104300440A980011,
    0x84B000400080201,
    0x7000C88102A0904,
    0x800004408802102,
];

/******************************************\
|==========================================|
|        Populating Magic Parameters       |
|==========================================|
\******************************************/

#[allow(long_running_const_eval)]
const fn populate_magic_table(pt: PieceType) -> MagicTable {
    let mut offset = 0;
    let mut magic = [Magic::EMPTY; Square::NUM];

    #[cfg(not(target_feature = "bmi2"))]
    let magic_numbers = match pt {
        PieceType::Bishop => BISHOP_MAGIC_NUMS,
        PieceType::Rook => ROOK_MAGIC_NUMS,
        _ => unreachable!(),
    };

    let mut i = 0;
    while i < Square::NUM {
        let sq = Square::from_unchecked(i as u8);

        let mask = Bitboard(
            Bitboard::attack_on_the_fly(pt, sq.bb(), Bitboard::EMPTY).0 & !get_edge_mask(sq).0,
        );
        #[cfg(not(target_feature = "bmi2"))]
        let shift = 64 - mask.count_bits() as u8;

        let m = Magic {
            #[cfg(not(target_feature = "bmi2"))]
            magic: magic_numbers[i],
            mask,
            #[cfg(not(target_feature = "bmi2"))]
            shift,
            offset,
        };

        magic[i] = m;

        offset += 1 << mask.count_bits();

        i += 1;
    }

    magic
}

fn populate_attack_table<const N: usize>(pt: PieceType) -> Vec<Bitboard> {
    let mut table = vec![Bitboard::EMPTY; N];

    let magics = match pt {
        PieceType::Rook => ROOK_MAGICS,
        PieceType::Bishop => BISHOP_MAGICS,
        _ => unreachable!(),
    };

    let mut i = 0;
    while i < Square::NUM {
        let m = magics[i];
        let sq = Square::from_unchecked(i as u8);

        let perm = 1 << m.mask.count_bits();

        let mut occ = Bitboard::EMPTY;
        let mut j = 0;
        while j < perm {
            table[m.index(occ)] = Bitboard::attack_on_the_fly(pt, sq.bb(), occ);

            occ = Bitboard((occ.0.wrapping_sub(m.mask.0)) & m.mask.0);

            j += 1;
        }

        i += 1;
    }
    table
}

/******************************************\
|==========================================|
|            Helper Functions              |
|==========================================|
\******************************************/

pub(crate) const fn get_edge_mask(sq: Square) -> Bitboard {
    use File::*;
    use Rank::*;

    let rank_18bb: Bitboard = Bitboard(Rank1.bb().0 | Rank8.bb().0);
    let file_ahbb: Bitboard = Bitboard(FileA.bb().0 | FileH.bb().0);

    let sq_rank_bb = sq.rank().bb();
    let sq_file_bb = sq.file().bb();

    let rank_mask = rank_18bb.0 & !sq_rank_bb.0;

    let file_mask = file_ahbb.0 & !sq_file_bb.0;

    Bitboard(rank_mask | file_mask)
}
