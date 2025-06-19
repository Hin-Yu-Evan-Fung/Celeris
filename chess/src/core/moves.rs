use crate::{board::Board, core::*};

/******************************************\
|==========================================|
|                   Move                   |
|==========================================|
\******************************************/

/// # Move Representation
/// Represents a chess move encoded into a 16-bit unsigned integer.
///
/// It stores the 'from' square, 'to' square, and special move flags within a single `u16`.
///
/// ## Encoding Format (6-6-4 Layout)
///
/// | Bits   | Range | Purpose                        |
/// |--------|-------|--------------------------------|
/// | 0-5    | 0-63  | 'From' square index (`Square`) |
/// | 6-11   | 0-63  | 'To' square index (`Square`)   |
/// | 12-15  | 0-15  | Flags (`MoveFlag` value)       |
///
///
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct Move {
    data: u16,
}

/******************************************\
|==========================================|
|                Move Flag                 |
|==========================================|
\******************************************/

/// Represents the type of a chess move, encoded in the upper 4 bits of a `Move`.
///
/// These flags distinguish between quiet moves, captures, promotions, castling, etc.
/// The specific values are chosen to allow efficient checking using bit masks.
///
/// https://www.chessprogramming.org/Encoding_Moves
#[repr(u16)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MoveFlag {
    QuietMove = 0b0000,
    DoublePawnPush = 0b0001,
    KingCastle = 0b0010,
    QueenCastle = 0b0011,
    Capture = 0b0100,
    EPCapture = 0b0101,
    KnightPromo = 0b1000,
    BishopPromo = 0b1001,
    RookPromo = 0b1010,
    QueenPromo = 0b1011,
    KnightPromoCapture = 0b1100,
    BishopPromoCapture = 0b1101,
    RookPromoCapture = 0b1110,
    QueenPromoCapture = 0b1111,
}

crate::impl_from_to_primitive!(MoveFlag, u16);

/******************************************\
|==========================================|
|              Implementation              |
|==========================================|
\******************************************/

impl MoveFlag {
    const CAPTURE_FLAG_MASK: u16 = 0x4;
    const PROMOTION_FLAG_MASK: u16 = 0x8;
    const PROMOTION_PIECE_MASK: u16 = 0x3;

    /// Returns whether the move flag is a capture move flag
    #[inline(always)]
    pub const fn is_capture(self) -> bool {
        (self as u16 & Self::CAPTURE_FLAG_MASK) != 0
    }

    /// Returns whether the move flag is a promotion moveflag
    #[inline(always)]
    pub const fn is_promotion(self) -> bool {
        (self as u16 & Self::PROMOTION_FLAG_MASK) != 0
    }

    /// Returns the promotion piece type of the move flag
    ///
    /// ## Safety
    ///
    /// Assumes the moveflag is a promotion, eg. `MoveFlag::KnightPromo`.
    #[inline(always)]
    pub const unsafe fn promotion_piece_type(self) -> PieceType {
        debug_assert!(self.is_promotion());
        let promo_index = (self as u16 & Self::PROMOTION_PIECE_MASK) as usize;

        match promo_index {
            0 => PieceType::Knight,
            1 => PieceType::Bishop,
            2 => PieceType::Rook,
            3 => PieceType::Queen,
            _ => unreachable!(),
        }
    }
}

impl Default for Move {
    fn default() -> Self {
        Self::NONE
    }
}

impl Move {
    // Move represetntation shifts
    const FROM_SHIFT: u16 = 0;
    const TO_SHIFT: u16 = 6;
    const FLAG_SHIFT: u16 = 12;
    const SQUARE_MASK: u16 = 0x3F;
    const FLAG_MASK: u16 = 0xF;

    /// Represents an invalid or uninitialized move.
    /// Often used to signify no move or a placeholder.
    pub const NONE: Self = Self::new(Square::A1, Square::A1, MoveFlag::QuietMove);
    /// Represents a null move.
    /// Used in chess engines for null move pruning.
    pub const NULL: Self = Self::new(Square::A2, Square::A2, MoveFlag::QuietMove);

    /// Create new move from row data
    #[inline]
    pub const unsafe fn new_raw(data: u16) -> Self {
        Self { data }
    }

    /// Create new move from information like from to and flag
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

    /// Returns the sourece square of the move
    #[inline(always)]
    pub const fn from(&self) -> Square {
        unsafe {
            Square::from_unchecked(((self.data >> Self::FROM_SHIFT) & Self::SQUARE_MASK) as u8)
        }
    }

    /// Returns the destination square of the move
    #[inline(always)]
    pub const fn to(&self) -> Square {
        unsafe { Square::from_unchecked(((self.data >> Self::TO_SHIFT) & Self::SQUARE_MASK) as u8) }
    }

    /// Returns the move flag of the move
    #[inline(always)]
    pub const fn flag(&self) -> MoveFlag {
        MoveFlag::from_unchecked((self.data >> Self::FLAG_SHIFT) & Self::FLAG_MASK)
    }

    /// Returns whether the move is a capture
    #[inline(always)]
    pub fn is_capture(&self) -> bool {
        self.flag().is_capture()
    }

    /// Returns whether the move is a promotion
    #[inline(always)]
    pub fn is_promotion(&self) -> bool {
        self.flag().is_promotion()
    }

    /// Returns whether the move is a castling move
    #[inline(always)]
    pub fn is_castle(&self) -> bool {
        self.flag() == MoveFlag::KingCastle || self.flag() == MoveFlag::QueenCastle
    }

    /// Returns the promotion piece type of the move
    ///
    /// ## Safety
    ///
    /// Assumes the move flag is a promotion, eg. KnightPromoCapture
    #[inline(always)]
    pub unsafe fn promotion_pt(&self) -> PieceType {
        debug_assert!(self.is_promotion());
        unsafe { self.flag().promotion_piece_type() }
    }

    /// Returns the raw data of the move
    #[inline(always)]
    pub const fn raw(&self) -> u16 {
        self.data
    }

    /// Returns whether the move is valid
    #[inline(always)]
    pub fn is_valid(self) -> bool {
        self != Self::NONE && self != Self::NULL
    }

    /// Returns the string representation of the move (UCI format)
    pub fn to_str(&self, board: &Board) -> String {
        if !self.is_valid() {
            "none".to_string()
        } else if board.chess960() && self.is_castle() {
            let to = match (board.stm(), self.flag()) {
                (Colour::White, MoveFlag::KingCastle) => board.rook_sq(Castling::WK),
                (Colour::White, MoveFlag::QueenCastle) => board.rook_sq(Castling::WQ),
                (Colour::Black, MoveFlag::KingCastle) => board.rook_sq(Castling::BK),
                (Colour::Black, MoveFlag::QueenCastle) => board.rook_sq(Castling::BQ),
                _ => unreachable!(),
            };
            format!("{}{}", self.from(), to)
        } else if self.is_promotion() {
            // Safety: the move is a promotion so it has a promotion pt and satisfies the constraints for the self.promotion_pt function
            unsafe { format!("{}{}{}", self.from(), self.to(), self.promotion_pt()) }
        } else {
            format!("{}{}", self.from(), self.to())
        }
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
    use crate::core::piece::PieceType;
    use crate::core::square::Square::*;

    #[test]
    fn test_encoding_decoding_basic() {
        let m = Move::new(E2, E4, MoveFlag::QuietMove);
        assert_eq!(m.from(), E2);
        assert_eq!(m.to(), E4);
        assert_eq!(m.flag(), MoveFlag::QuietMove);
        assert!(m.flag() == MoveFlag::QuietMove);
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
        assert!(m.flag() != MoveFlag::QuietMove);
        assert!(m.is_capture());
        assert!(!m.is_promotion());
        assert!(m.flag() != MoveFlag::EPCapture);
    }

    #[test]
    fn test_encoding_decoding_ep_capture() {
        let m = Move::new(E5, D6, MoveFlag::EPCapture);
        assert_eq!(m.from(), E5);
        assert_eq!(m.to(), D6);
        assert_eq!(m.flag(), MoveFlag::EPCapture);
        assert!(m.flag() != MoveFlag::QuietMove);
        assert!(m.is_capture());
        assert!(m.flag() == MoveFlag::EPCapture);
        assert!(!m.is_promotion());
    }

    #[test]
    fn test_encoding_decoding_double_push() {
        let m = Move::new(A2, A4, MoveFlag::DoublePawnPush);
        assert_eq!(m.from(), A2);
        assert_eq!(m.to(), A4);
        assert_eq!(m.flag(), MoveFlag::DoublePawnPush);
        assert!(m.flag() == MoveFlag::DoublePawnPush);
        assert!(!m.is_capture());
        assert!(!m.is_promotion());
    }

    #[test]
    fn test_encoding_decoding_castles() {
        let m_ks = Move::new(E1, G1, MoveFlag::KingCastle);
        assert_eq!(m_ks.from(), E1);
        assert_eq!(m_ks.to(), G1);
        assert_eq!(m_ks.flag(), MoveFlag::KingCastle);
        assert!(m_ks.flag() == MoveFlag::KingCastle);
        assert!(m_ks.flag() != MoveFlag::QueenCastle);
        assert!(m_ks.is_castle());
        assert!(!m_ks.is_capture());

        let m_qs = Move::new(E8, C8, MoveFlag::QueenCastle);
        assert_eq!(m_qs.from(), E8);
        assert_eq!(m_qs.to(), C8);
        assert_eq!(m_qs.flag(), MoveFlag::QueenCastle);
        assert!(m_qs.flag() != MoveFlag::KingCastle);
        assert!(m_qs.flag() == MoveFlag::QueenCastle);
        assert!(m_qs.is_castle());
        assert!(!m_qs.is_capture());
    }

    #[test]
    fn test_promotion_constructor_quiet() {
        let m_qn = Move::new(A7, A8, MoveFlag::KnightPromo);
        assert_eq!(m_qn.from(), A7);
        assert_eq!(m_qn.to(), A8);
        assert_eq!(m_qn.flag(), MoveFlag::KnightPromo);
        assert!(m_qn.is_promotion());
        assert!(!m_qn.is_capture());
        assert_eq!(unsafe { m_qn.promotion_pt() }, PieceType::Knight);

        // let m_qb = Move::new_promotion(B7, B8, PieceType::Bishop, false);
        let m_qb = Move::new(B7, B8, MoveFlag::BishopPromo);
        assert_eq!(m_qb.flag(), MoveFlag::BishopPromo);
        assert_eq!(unsafe { m_qb.promotion_pt() }, PieceType::Bishop);

        // let m_qr = Move::new_promotion(C7, C8, PieceType::Rook, false);
        let m_qr = Move::new(C7, C8, MoveFlag::RookPromo);
        assert_eq!(m_qr.flag(), MoveFlag::RookPromo);
        assert_eq!(unsafe { m_qr.promotion_pt() }, PieceType::Rook);

        let m_qq = Move::new(D7, D8, MoveFlag::QueenPromo);
        assert_eq!(m_qq.flag(), MoveFlag::QueenPromo);
        assert_eq!(unsafe { m_qq.promotion_pt() }, PieceType::Queen);
    }

    #[test]
    fn test_promotion_constructor_capture() {
        let m_cn = Move::new(A7, B8, MoveFlag::KnightPromoCapture);
        assert_eq!(m_cn.from(), A7);
        assert_eq!(m_cn.to(), B8);
        assert_eq!(m_cn.flag(), MoveFlag::KnightPromoCapture);
        assert!(m_cn.is_promotion());
        assert!(m_cn.is_capture());
        assert_eq!(unsafe { m_cn.promotion_pt() }, PieceType::Knight);

        let m_cb = Move::new(B8, C7, MoveFlag::BishopPromoCapture);
        assert_eq!(m_cb.flag(), MoveFlag::BishopPromoCapture);
        assert_eq!(unsafe { m_cb.promotion_pt() }, PieceType::Bishop);

        let m_cr = Move::new(C7, D8, MoveFlag::RookPromoCapture);
        assert_eq!(m_cr.flag(), MoveFlag::RookPromoCapture);
        assert_eq!(unsafe { m_cr.promotion_pt() }, PieceType::Rook);

        let m_cq = Move::new(D7, E8, MoveFlag::QueenPromoCapture);
        assert_eq!(m_cq.flag(), MoveFlag::QueenPromoCapture);
        assert_eq!(unsafe { m_cq.promotion_pt() }, PieceType::Queen);
    }

    #[test]
    fn test_query_methods_comprehensive() {
        let quiet = Move::new(G1, F3, MoveFlag::QuietMove);
        let capture = Move::new(E4, D5, MoveFlag::Capture);
        let ep = Move::new(E5, D6, MoveFlag::EPCapture);
        let ks_castle = Move::new(E1, G1, MoveFlag::KingCastle);
        let qs_castle = Move::new(E8, C8, MoveFlag::QueenCastle);
        let dpp = Move::new(A2, A4, MoveFlag::DoublePawnPush);

        assert!(quiet.flag() == MoveFlag::QuietMove);
        assert!(capture.flag() != MoveFlag::QuietMove);
        assert!(ep.flag() != MoveFlag::QuietMove);
        assert!(ks_castle.flag() != MoveFlag::QuietMove);
        assert!(qs_castle.flag() != MoveFlag::QuietMove);
        assert!(dpp.flag() != MoveFlag::QuietMove);

        assert!(!quiet.is_capture());
        assert!(capture.is_capture());
        assert!(ep.is_capture());
        assert!(!ks_castle.is_capture());
        assert!(!qs_castle.is_capture());
        assert!(!dpp.is_capture());

        assert!(!quiet.is_promotion());
        assert!(!capture.is_promotion());
        assert!(!ep.is_promotion());
        assert!(!ks_castle.is_promotion());
        assert!(!qs_castle.is_promotion());
        assert!(!dpp.is_promotion());

        assert!(quiet.flag() != MoveFlag::EPCapture);
        assert!(capture.flag() != MoveFlag::EPCapture);
        assert!(ep.flag() == MoveFlag::EPCapture);

        assert!(!quiet.is_castle());
        assert!(ks_castle.flag() == MoveFlag::KingCastle);
        assert!(ks_castle.is_castle());
        assert!(ks_castle.flag() != MoveFlag::QueenCastle);
        assert!(qs_castle.flag() == MoveFlag::QueenCastle);
        assert!(qs_castle.is_castle());
        assert!(qs_castle.flag() != MoveFlag::KingCastle);

        assert!(quiet.flag() != MoveFlag::DoublePawnPush);
        assert!(dpp.flag() == MoveFlag::DoublePawnPush);
        assert!(capture.flag() != MoveFlag::DoublePawnPush);
    }

    #[test]
    fn test_move_equality() {
        let m1 = Move::new(E2, E4, MoveFlag::DoublePawnPush);
        let m2 = Move::new(E2, E4, MoveFlag::DoublePawnPush);
        let m3 = Move::new(E2, E3, MoveFlag::QuietMove);
        let m4 = Move::new(E2, E4, MoveFlag::QuietMove);

        assert_eq!(m1, m2);
        assert_ne!(m1, m3);
        assert_ne!(m1, m4);
    }

    #[test]
    fn test_default_move() {
        let default_move = Move::default();
        assert_eq!(default_move.data, 0);
        assert_eq!(default_move.from(), A1);
        assert_eq!(default_move.to(), A1);
        assert_eq!(default_move.flag(), MoveFlag::QuietMove);
        assert!(default_move.flag() == MoveFlag::QuietMove);
    }
}
