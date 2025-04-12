//! # Module: `magic_numbers`
//!
//! This module provides precomputed magic numbers for rook and bishop attacks.
//! These magic numbers are used to efficiently calculate sliding piece attacks
//! on the fly, without the need for iterative calculations.
//!
//! ## Overview
//!
//! Magic numbers are a technique used in chess engines to quickly determine
//! the squares attacked by sliding pieces (rooks, bishops, and queens).
//! Each square on the board has a unique magic number associated with it.
//! By multiplying the magic number with a bitboard representing the occupancy
//! of the board, and then right-shifting the result, we can obtain an index
//! into a precomputed attack table. This index directly gives us the bitboard
//! of squares attacked by the sliding piece from that square, given the current
//! occupancy.
//!
//! ## Constants
//!
//! - `BISHOP_MAGICS`: An array of 64 magic numbers, one for each square on the board,
//!   for calculating bishop attacks.
//! - `ROOK_MAGICS`: An array of 64 magic numbers, one for each square on the board,
//!   for calculating rook attacks.
//!
//! ## Usage
//!
//! These magic numbers are used in the `movegen` module, specifically in the
//! `slider_attack` function, to efficiently calculate the attacks of sliding
//! pieces. They are essential for the performance of the chess engine, as they
//! allow for rapid move generation and evaluation.
//!
//! ## Implementation Details
//!
//! The magic numbers are carefully chosen to minimize collisions (i.e., different
//! occupancies mapping to the same index) while keeping the size of the attack
//! tables manageable. The process of finding these magic numbers is computationally
//! intensive and is typically done offline. The `magic_gen` module contains the
//! code used to generate these magic numbers.

use crate::core::Square;

/******************************************\
|==========================================|
|              Magic Numbers               |
|==========================================|
\******************************************/

pub(super) const BISHOP_MAGICS: [u64; Square::NUM] = [
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

pub(super) const ROOK_MAGICS: [u64; Square::NUM] = [
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
