//! # Module: `moves`
//!
//! Defines the representation and manipulation of chess moves within the engine.
//!
//! ## Overview
//!
//! This module provides the `Move` struct, which encodes a chess move into a compact 16-bit
//! format, and the `MoveFlag` enum, which categorizes different types of moves (quiet,
//! capture, castling, promotion, etc.). This compact representation is crucial for performance,
//! especially during move generation and search, as it reduces memory usage and improves cache
//! efficiency.
//!
//! The core components are:
//! - **`MoveFlag`**: An enum representing the type of move (Quiet, Capture, Promotion, etc.).
//! - **`Move`**: A struct wrapping a `u16` that encodes the 'from' square, 'to' square,
//!   and the `MoveFlag`.
//!
//! Helper methods are provided on both `MoveFlag` and `Move` to easily query properties
//! like whether a move is a capture, promotion, or castle, and to extract components like
//! the squares or promotion piece type.
//!
//! ## Encoding Scheme (16-bit `Move`)
//!
//! The `Move` struct uses a 6-6-4 layout within its internal `u16`:
//!
//! | Bits   | Range | Purpose                     | Extracted By |
//! |--------|-------|-----------------------------|--------------|
//! | 0-5    | 0-63  | 'From' square index (`Square`) | `Move::from_unchecked()` |
//! | 6-11   | 0-63  | 'To' square index (`Square`)   | `Move::to()`   |
//! | 12-15  | 0-15  | Flags (`MoveFlag` value)    | `Move::flag()` |
//!
//! The flags (bits 12-15) encode special move types like castling, en passant, captures,
//! double pawn pushes, and promotions (including the promoted piece type). See `MoveFlag`
//! documentation for details on specific flag values and their meaning.
//!
//! ## Usage
//!
//! Moves are typically created using `Move::new()` or specialized constructors like
//! `Move::new_promotion()`. Properties can then be queried using methods like `is_capture()`,
//! `promotion_piece_type()`, etc. The `Display` trait is implemented for basic algebraic
//! notation output (e.g., "e2e4", "a7a8q").

use macros::FromPrimitive;

use crate::core::*;

/// Represents the type of a chess move, encoded in the upper 4 bits of a `Move`.
///
/// These flags distinguish between quiet moves, captures, promotions, castling, etc.
/// The specific values are chosen to allow efficient checking using bit masks.
///
/// - **Bit 2 (0x4):** Set for any capture (Capture, EPCapture, PromoCapture variants).
/// - **Bit 3 (0x8):** Set for any promotion (Promo, PromoCapture variants).
/// - **Bits 0-1 (0x3):** Used within promotion flags to indicate the piece type (N=0, B=1, R=2, Q=3).
#[repr(u16)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, FromPrimitive)]
pub enum MoveFlag {
    /// A standard move without capture or other special properties. Value: 0 (0b0000)
    QuietMove = 0b0000,
    /// A pawn moving two squares forward from its starting rank. Value: 1 (0b0001)
    DoublePawnPush = 0b0001,
    /// Kingside castling (O-O). Value: 2 (0b0010)
    KingCastle = 0b0010,
    /// Queenside castling (O-O-O). Value: 3 (0b0011)
    QueenCastle = 0b0011,
    /// A standard capture move (excluding en passant and promotion captures). Value: 4 (0b0100)
    Capture = 0b0100,
    /// En passant capture. Value: 5 (0b0101)
    EPCapture = 0b0101,
    // Values 6 (0b0110) and 7 (0b0111) are unused.
    /// Promotion to a Knight without capture. Value: 8 (0b1000)
    KnightPromo = 0b1000,
    /// Promotion to a Bishop without capture. Value: 9 (0b1001)
    BishopPromo = 0b1001,
    /// Promotion to a Rook without capture. Value: 10 (0b1010)
    RookPromo = 0b1010,
    /// Promotion to a Queen without capture. Value: 11 (0b1011)
    QueenPromo = 0b1011,
    /// Promotion to a Knight with capture. Value: 12 (0b1100)
    KnightPromoCapture = 0b1100,
    /// Promotion to a Bishop with capture. Value: 13 (0b1101)
    BishopPromoCapture = 0b1101,
    /// Promotion to a Rook with capture. Value: 14 (0b1110)
    RookPromoCapture = 0b1110,
    /// Promotion to a Queen with capture. Value: 15 (0b1111)
    QueenPromoCapture = 0b1111,
}

impl MoveFlag {
    /// Mask to isolate the capture bit (bit 2).
    const CAPTURE_FLAG_MASK: u16 = 0x4; // 0b0100
    /// Mask to isolate the promotion bit (bit 3).
    const PROMOTION_FLAG_MASK: u16 = 0x8; // 0b1000
    /// Mask to isolate the promotion piece type bits (bits 0-1).
    const PROMOTION_PIECE_MASK: u16 = 0x3; // 0b0011

    /// Checks if this flag represents any type of capture (standard, en passant, or promotion capture).
    ///
    /// This checks if the capture bit (0b0100) is set in the flag's value.
    #[inline(always)]
    pub const fn is_capture(self) -> bool {
        (self as u16 & Self::CAPTURE_FLAG_MASK) != 0
    }

    /// Checks if this flag represents any type of promotion (quiet or capture).
    ///
    /// This checks if the promotion bit (0b1000) is set in the flag's value.
    #[inline(always)]
    pub const fn is_promotion(self) -> bool {
        (self as u16 & Self::PROMOTION_FLAG_MASK) != 0
    }

    /// Gets the promotion piece type if this flag represents a promotion.
    ///
    /// ### **Warning**: Behaviour is not defined when the move itself is not a PROMOTION
    ///
    /// Returns `None` if the flag is not a promotion flag. Otherwise, extracts the
    /// piece type (Knight, Bishop, Rook, Queen) from the lower 2 bits of the flag value.
    #[inline(always)]
    pub const fn promotion_piece_type(self) -> PieceType {
        // Extract the piece type index from the lower 2 bits (0b0011)
        let promo_index = (self as u16 & Self::PROMOTION_PIECE_MASK) as usize;
        // Map index to PieceType
        match promo_index {
            0 => PieceType::Knight, // 0bxx00
            1 => PieceType::Bishop, // 0bxx01
            2 => PieceType::Rook,   // 0bxx10
            3 => PieceType::Queen,  // 0bxx11
            _ => unreachable!(),    // Should not happen with valid flags
        }
    }

    pub const fn promotion_flag(piece_type: PieceType, is_capture: bool) -> MoveFlag {
        match (piece_type, is_capture) {
            (PieceType::Knight, false) => MoveFlag::KnightPromo,
            (PieceType::Bishop, false) => MoveFlag::BishopPromo,
            (PieceType::Rook, false) => MoveFlag::RookPromo,
            (PieceType::Queen, false) => MoveFlag::QueenPromo,
            (PieceType::Knight, true) => MoveFlag::KnightPromoCapture,
            (PieceType::Bishop, true) => MoveFlag::BishopPromoCapture,
            (PieceType::Rook, true) => MoveFlag::RookPromoCapture,
            (PieceType::Queen, true) => MoveFlag::QueenPromoCapture,
            _ => panic!("Invalid promotion piece type!"),
        }
    }
}
/// # Move Representation

/// Represents a chess move encoded into a 16-bit unsigned integer.
///
/// This struct provides a compact representation suitable for high-performance chess engines.
/// It stores the 'from' square, 'to' square, and special move flags within a single `u16`.
///
/// ## Encoding Format (6-6-4 Layout)
///
/// | Bits   | Range | Purpose                     |
/// |--------|-------|-----------------------------|
/// | 0-5    | 0-63  | 'From' square index (`Square`) |
/// | 6-11   | 0-63  | 'To' square index (`Square`)   |
/// | 12-15  | 0-15  | Flags (`MoveFlag` value)    |
///
/// Use the associated methods (`from()`, `to()`, `flag()`, `is_capture()`, etc.) to access
/// the move's components and properties.
#[derive(Debug, PartialEq, Clone, Copy, Eq, Default)] // Default makes Move(0) - often represents a null move
pub struct Move {
    data: u16,
}

impl Move {
    // --- Bit Manipulation Constants ---
    /// Bit shift for the 'from' square (bits 0-5).
    const FROM_SHIFT: u16 = 0;
    /// Bit shift for the 'to' square (bits 6-11).
    const TO_SHIFT: u16 = 6;
    /// Bit shift for the flags (bits 12-15).
    const FLAG_SHIFT: u16 = 12;
    /// Mask to extract the square value (6 bits).
    const SQUARE_MASK: u16 = 0x3F; // 0b00000000111111
    /// Mask to extract the flag value (4 bits).
    const FLAG_MASK: u16 = 0xF; // 0b00000000001111

    // --- Default Moves ---
    /// Null Move Placeholder
    pub const NULL: Self = Self::new(Square::A1, Square::A1, MoveFlag::QuietMove);

    /// Creates a new move from its components.
    ///
    /// # Arguments
    /// * `from` - The starting square (`Square`).
    /// * `to` - The destination square (`Square`).
    /// * `flag` - The `MoveFlag` specifying the type of move.
    #[inline(always)]
    pub const fn new(from: Square, to: Square, flag: MoveFlag) -> Self {
        let from_u16 = from as u16;
        let to_u16 = to as u16;
        let flag_u16 = flag as u16;

        let data = (from_u16 << Self::FROM_SHIFT)
            | (to_u16 << Self::TO_SHIFT)
            | (flag_u16 << Self::FLAG_SHIFT);

        Self { data }
    }

    /// # Creates a promotion move (quiet or capture).
    ///
    /// Determines the correct `MoveFlag` based on the target piece type and whether
    /// it's a capture.
    ///
    /// # Arguments
    /// * `from` - The starting square (pawn's current square).
    /// * `to` - The destination square (on rank 1 or 8).
    /// * `piece_type` - The `PieceType` to promote to (Knight, Bishop, Rook, or Queen).
    /// * `is_capture` - `true` if this promotion also captures a piece.
    ///
    /// # Panics
    /// Panics if `piece_type` is not `Knight`, `Bishop`, `Rook`, or `Queen`.
    #[inline(always)]
    pub fn new_promotion(
        from: Square,
        to: Square,
        piece_type: PieceType,
        is_capture: bool,
    ) -> Self {
        Self::new(from, to, MoveFlag::promotion_flag(piece_type, is_capture))
    }

    // --- Accessors ---

    /// Gets the 'from' square.
    #[inline(always)]
    pub const fn from(&self) -> Square {
        // Extract bits 0-5 and convert to Square
        unsafe {
            Square::from_unchecked(((self.data >> Self::FROM_SHIFT) & Self::SQUARE_MASK) as u8)
        }
    }

    /// Gets the 'to' square.
    #[inline(always)]
    pub const fn to(&self) -> Square {
        // Extract bits 6-11 and convert to Square
        unsafe { Square::from_unchecked(((self.data >> Self::TO_SHIFT) & Self::SQUARE_MASK) as u8) }
    }

    /// # Gets the `MoveFlag` specifying the type of this move.
    ///
    /// Returns `MoveFlag` variant in the moves
    #[inline(always)]
    pub const fn flag(&self) -> MoveFlag {
        unsafe { MoveFlag::from_unchecked((self.data >> Self::FLAG_SHIFT) & Self::FLAG_MASK) }
    }

    // --- Query Methods ---

    /// Checks if the move is any type of capture
    #[inline(always)]
    pub(crate) const fn is_capture(&self) -> bool {
        // Get the flag (if valid) and check its capture property
        self.flag().is_capture()
    }

    /// Checks if the move is any type of promotion
    #[inline(always)]
    pub(crate) const fn is_promotion(&self) -> bool {
        // Get the flag (if valid) and check its promotion property
        self.flag().is_promotion()
    }

    /// Gets the type of piece this move promotes to, if it's a promotion move.
    ///
    /// Returns `None` if the move is not a promotion.
    #[inline(always)]
    pub(crate) const fn promotion_pt(&self) -> PieceType {
        // Get the flag (if valid) and then get its promotion piece type
        self.flag().promotion_piece_type()
    }

    /// Checks if the move is a quiet move
    #[inline(always)]
    pub(crate) const fn is_quiet(&self) -> bool {
        self.flag() as u8 == MoveFlag::QuietMove as u8
    }

    /// Checks if the move is a double push
    #[inline(always)]
    pub(crate) const fn is_double_push(&self) -> bool {
        self.flag() as u8 == MoveFlag::DoublePawnPush as u8
    }

    /// Checks if the move is an enpassant capture
    #[inline(always)]
    pub(crate) const fn is_ep_capture(&self) -> bool {
        self.flag() as u8 == MoveFlag::EPCapture as u8
    }

    /// Checks if the move is a king side castle move
    #[inline(always)]
    pub(crate) const fn is_king_castle(&self) -> bool {
        self.flag() as u8 == MoveFlag::KingCastle as u8
    }

    /// Checks if the move is a queen side castle move
    #[inline(always)]
    pub(crate) const fn is_queen_castle(&self) -> bool {
        self.flag() as u8 == MoveFlag::QueenCastle as u8
    }

    /// Checks if the move is a castle move
    #[inline(always)]
    pub(crate) const fn is_castle(&self) -> bool {
        self.is_king_castle() || self.is_queen_castle()
    }

    /// Checks if the move is a null move
    #[inline(always)]
    pub(crate) const fn is_null(&self) -> bool {
        self.data == Self::NULL.data
    }
}

/// Formats the move in standard algebraic notation (e.g., "e2e4", "a7a8q").
///
/// Note: This basic implementation does not include capture 'x' or check '+' symbols.
/// It simply shows the 'from' square, 'to' square, and the promotion piece character if applicable.
impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_null() {
            write!(f, "null")
        } else {
            // Check if it's a promotion move to append the piece character
            if self.is_promotion() {
                // Get the lowercase character for the promotion piece
                // Assumes PieceType Display implementation gives lowercase 'n', 'b', 'r', 'q'
                // Or implement a specific mapping here:
                write!(
                    f,
                    "{}{}{}",
                    self.from(),
                    self.to(),
                    self.promotion_pt().to_string()
                )
            } else {
                // Standard move format
                write!(f, "{}{}", self.from(), self.to())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import Move, MoveFlag, etc.
    use crate::core::piece::PieceType;
    use crate::core::square::Square::*; // Import square variants for easy use // Ensure PieceType is in scope

    #[test]
    fn test_encoding_decoding_basic() {
        // Test a simple quiet move (using the flag directly)
        let m = Move::new(E2, E4, MoveFlag::QuietMove);
        assert_eq!(m.from(), E2);
        assert_eq!(m.to(), E4);
        assert_eq!(m.flag(), MoveFlag::QuietMove);
        assert!(m.is_quiet());
        assert!(!m.is_capture());
        assert!(!m.is_promotion());
        assert!(!m.is_castle());
    }

    #[test]
    fn test_encoding_decoding_capture() {
        let m = Move::new(D4, C5, MoveFlag::Capture);
        assert_eq!(m.from(), D4);
        assert_eq!(m.to(), C5);
        assert_eq!(m.flag(), MoveFlag::Capture);
        assert!(!m.is_quiet());
        assert!(m.is_capture());
        assert!(!m.is_promotion());
        assert!(!m.is_ep_capture());
    }

    #[test]
    fn test_encoding_decoding_ep_capture() {
        let m = Move::new(E5, D6, MoveFlag::EPCapture);
        assert_eq!(m.from(), E5);
        assert_eq!(m.to(), D6);
        assert_eq!(m.flag(), MoveFlag::EPCapture);
        assert!(!m.is_quiet());
        assert!(m.is_capture()); // EP is a capture
        assert!(m.is_ep_capture());
        assert!(!m.is_promotion());
    }

    #[test]
    fn test_encoding_decoding_double_push() {
        let m = Move::new(A2, A4, MoveFlag::DoublePawnPush);
        assert_eq!(m.from(), A2);
        assert_eq!(m.to(), A4);
        assert_eq!(m.flag(), MoveFlag::DoublePawnPush);
        assert!(m.is_double_push());
        assert!(!m.is_capture());
        assert!(!m.is_promotion());
    }

    #[test]
    fn test_encoding_decoding_castles() {
        let m_ks = Move::new(E1, G1, MoveFlag::KingCastle);
        assert_eq!(m_ks.from(), E1);
        assert_eq!(m_ks.to(), G1);
        assert_eq!(m_ks.flag(), MoveFlag::KingCastle);
        assert!(m_ks.is_king_castle());
        assert!(!m_ks.is_queen_castle());
        assert!(m_ks.is_castle());
        assert!(!m_ks.is_capture());

        let m_qs = Move::new(E8, C8, MoveFlag::QueenCastle);
        assert_eq!(m_qs.from(), E8);
        assert_eq!(m_qs.to(), C8);
        assert_eq!(m_qs.flag(), MoveFlag::QueenCastle);
        assert!(!m_qs.is_king_castle());
        assert!(m_qs.is_queen_castle());
        assert!(m_qs.is_castle());
        assert!(!m_qs.is_capture());
    }

    #[test]
    fn test_promotion_constructor_quiet() {
        let m_qn = Move::new_promotion(A7, A8, PieceType::Knight, false);
        assert_eq!(m_qn.from(), A7);
        assert_eq!(m_qn.to(), A8);
        assert_eq!(m_qn.flag(), MoveFlag::KnightPromo);
        assert!(m_qn.is_promotion());
        assert!(!m_qn.is_capture());
        assert_eq!(m_qn.promotion_pt(), PieceType::Knight);

        let m_qb = Move::new_promotion(B7, B8, PieceType::Bishop, false);
        assert_eq!(m_qb.flag(), MoveFlag::BishopPromo);
        assert_eq!(m_qb.promotion_pt(), PieceType::Bishop);

        let m_qr = Move::new_promotion(C7, C8, PieceType::Rook, false);
        assert_eq!(m_qr.flag(), MoveFlag::RookPromo);
        assert_eq!(m_qr.promotion_pt(), PieceType::Rook);

        let m_qq = Move::new_promotion(D7, D8, PieceType::Queen, false);
        assert_eq!(m_qq.flag(), MoveFlag::QueenPromo);
        assert_eq!(m_qq.promotion_pt(), PieceType::Queen);
    }

    #[test]
    fn test_promotion_constructor_capture() {
        let m_cn = Move::new_promotion(A7, B8, PieceType::Knight, true);
        assert_eq!(m_cn.from(), A7);
        assert_eq!(m_cn.to(), B8);
        assert_eq!(m_cn.flag(), MoveFlag::KnightPromoCapture);
        assert!(m_cn.is_promotion());
        assert!(m_cn.is_capture());
        assert_eq!(m_cn.promotion_pt(), PieceType::Knight);

        let m_cb = Move::new_promotion(B7, C8, PieceType::Bishop, true);
        assert_eq!(m_cb.flag(), MoveFlag::BishopPromoCapture);
        assert_eq!(m_cb.promotion_pt(), PieceType::Bishop);

        let m_cr = Move::new_promotion(C7, D8, PieceType::Rook, true);
        assert_eq!(m_cr.flag(), MoveFlag::RookPromoCapture);
        assert_eq!(m_cr.promotion_pt(), PieceType::Rook);

        let m_cq = Move::new_promotion(D7, E8, PieceType::Queen, true);
        assert_eq!(m_cq.flag(), MoveFlag::QueenPromoCapture);
        assert_eq!(m_cq.promotion_pt(), PieceType::Queen);
    }

    #[test]
    #[should_panic(expected = "Invalid promotion piece type!")]
    fn test_invalid_promotion_panic_pawn() {
        // Test that promoting to a Pawn panics
        Move::new_promotion(A7, A8, PieceType::Pawn, false);
    }

    #[test]
    #[should_panic(expected = "Invalid promotion piece type!")]
    fn test_invalid_promotion_panic_king() {
        // Test that promoting to a King panics
        Move::new_promotion(A7, A8, PieceType::King, false);
    }

    #[test]
    fn test_query_methods_comprehensive() {
        // Create samples for each flag type
        let quiet = Move::new(G1, F3, MoveFlag::QuietMove);
        let capture = Move::new(E4, D5, MoveFlag::Capture);
        let ep = Move::new(E5, D6, MoveFlag::EPCapture);
        let promo_cap = Move::new_promotion(B7, C8, PieceType::Queen, true);
        let promo_quiet = Move::new_promotion(A7, A8, PieceType::Rook, false);
        let ks_castle = Move::new(E1, G1, MoveFlag::KingCastle);
        let qs_castle = Move::new(E8, C8, MoveFlag::QueenCastle);
        let dpp = Move::new(A2, A4, MoveFlag::DoublePawnPush);

        // is_quiet
        assert!(quiet.is_quiet());
        assert!(!capture.is_quiet());
        assert!(!ep.is_quiet());
        assert!(!promo_cap.is_quiet());
        assert!(!promo_quiet.is_quiet());
        assert!(!ks_castle.is_quiet());
        assert!(!qs_castle.is_quiet());
        assert!(!dpp.is_quiet());

        // is_capture
        assert!(!quiet.is_capture());
        assert!(capture.is_capture());
        assert!(ep.is_capture());
        assert!(promo_cap.is_capture());
        assert!(!promo_quiet.is_capture());
        assert!(!ks_castle.is_capture());
        assert!(!qs_castle.is_capture());
        assert!(!dpp.is_capture());

        // is_promotion
        assert!(!quiet.is_promotion());
        assert!(!capture.is_promotion());
        assert!(!ep.is_promotion());
        assert!(promo_cap.is_promotion());
        assert!(promo_quiet.is_promotion());
        assert!(!ks_castle.is_promotion());
        assert!(!qs_castle.is_promotion());
        assert!(!dpp.is_promotion());

        // promotion_piece_type
        assert_eq!(promo_cap.promotion_pt(), PieceType::Queen);
        assert_eq!(promo_quiet.promotion_pt(), PieceType::Rook);

        // is_ep_capture
        assert!(!quiet.is_ep_capture());
        assert!(!capture.is_ep_capture());
        assert!(ep.is_ep_capture());
        assert!(!promo_cap.is_ep_capture());
        // ... and so on for other move types

        // is_king_castle / is_queen_castle / is_castle
        assert!(!quiet.is_castle());
        assert!(ks_castle.is_king_castle());
        assert!(ks_castle.is_castle());
        assert!(!ks_castle.is_queen_castle());
        assert!(qs_castle.is_queen_castle());
        assert!(qs_castle.is_castle());
        assert!(!qs_castle.is_king_castle());

        // is_double_push
        assert!(!quiet.is_double_push());
        assert!(dpp.is_double_push());
        assert!(!capture.is_double_push());
    }

    #[test]
    fn test_display_format() {
        // Use Move::new for non-promotion moves as helper constructors aren't present
        assert_eq!(Move::new(E2, E4, MoveFlag::QuietMove).to_string(), "e2e4");
        assert_eq!(Move::new(E4, D5, MoveFlag::Capture).to_string(), "e4d5"); // Display doesn't show capture 'x'
        assert_eq!(Move::new(E5, D6, MoveFlag::EPCapture).to_string(), "e5d6");
        assert_eq!(Move::new(E1, G1, MoveFlag::KingCastle).to_string(), "e1g1");

        // Use new_promotion for promotion moves
        assert_eq!(
            Move::new_promotion(A7, A8, PieceType::Queen, false).to_string(),
            "a7a8q"
        );
        assert_eq!(
            Move::new_promotion(B7, C8, PieceType::Knight, true).to_string(),
            "b7c8n"
        );
        assert_eq!(
            Move::new_promotion(G7, G8, PieceType::Rook, false).to_string(),
            "g7g8r"
        );
        assert_eq!(
            Move::new_promotion(H7, G8, PieceType::Bishop, true).to_string(),
            "h7g8b"
        );
    }

    #[test]
    fn test_move_equality() {
        let m1 = Move::new(E2, E4, MoveFlag::DoublePawnPush);
        let m2 = Move::new(E2, E4, MoveFlag::DoublePawnPush);
        let m3 = Move::new(E2, E3, MoveFlag::QuietMove);
        let m4 = Move::new(E2, E4, MoveFlag::QuietMove); // Same squares, different flag

        assert_eq!(m1, m2);
        assert_ne!(m1, m3);
        assert_ne!(m1, m4);
    }

    #[test]
    fn test_default_move() {
        let default_move = Move::default();
        assert_eq!(default_move.data, 0); // Raw value should be 0
        assert_eq!(default_move.from(), A1); // Square 0
        assert_eq!(default_move.to(), A1); // Square 0
        assert_eq!(default_move.flag(), MoveFlag::QuietMove); // Flag 0
        assert!(default_move.is_quiet());
    }
}
