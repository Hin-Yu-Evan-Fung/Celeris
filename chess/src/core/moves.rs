use crate::{board::Board, core::*};

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

impl MoveFlag {
    const CAPTURE_FLAG_MASK: u16 = 0x4;

    const PROMOTION_FLAG_MASK: u16 = 0x8;

    const PROMOTION_PIECE_MASK: u16 = 0x3;

    #[inline(always)]
    pub const fn is_capture(self) -> bool {
        (self as u16 & Self::CAPTURE_FLAG_MASK) != 0
    }

    #[inline(always)]
    pub const fn is_promotion(self) -> bool {
        (self as u16 & Self::PROMOTION_FLAG_MASK) != 0
    }

    #[inline(always)]
    pub const fn promotion_piece_type(self) -> PieceType {
        let promo_index = (self as u16 & Self::PROMOTION_PIECE_MASK) as usize;

        match promo_index {
            0 => PieceType::Knight,
            1 => PieceType::Bishop,
            2 => PieceType::Rook,
            3 => PieceType::Queen,
            _ => unreachable!(),
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

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct Move {
    data: u16,
}

impl Default for Move {
    fn default() -> Self {
        Self::NONE
    }
}

impl Move {
    const FROM_SHIFT: u16 = 0;

    const TO_SHIFT: u16 = 6;

    const FLAG_SHIFT: u16 = 12;

    const SQUARE_MASK: u16 = 0x3F;

    const FLAG_MASK: u16 = 0xF;

    pub const NONE: Self = Self::new(Square::A1, Square::A1, MoveFlag::QuietMove);

    pub const NULL: Self = Self::new(Square::A2, Square::A2, MoveFlag::QuietMove);

    #[inline]
    pub const unsafe fn new_raw(data: u16) -> Self {
        Self { data }
    }

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

    #[inline(always)]
    pub fn new_promotion(
        from: Square,
        to: Square,
        piece_type: PieceType,
        is_capture: bool,
    ) -> Self {
        Self::new(from, to, MoveFlag::promotion_flag(piece_type, is_capture))
    }

    #[inline(always)]
    pub const fn from(&self) -> Square {
        Square::from_unchecked(((self.data >> Self::FROM_SHIFT) & Self::SQUARE_MASK) as u8)
    }

    #[inline(always)]
    pub const fn to(&self) -> Square {
        Square::from_unchecked(((self.data >> Self::TO_SHIFT) & Self::SQUARE_MASK) as u8)
    }

    #[inline(always)]
    pub const fn flag(&self) -> MoveFlag {
        MoveFlag::from_unchecked((self.data >> Self::FLAG_SHIFT) & Self::FLAG_MASK)
    }

    #[inline(always)]
    pub const fn is_capture(&self) -> bool {
        self.flag().is_capture()
    }

    #[inline(always)]
    pub const fn is_promotion(&self) -> bool {
        self.flag().is_promotion()
    }

    #[inline(always)]
    pub const fn promotion_pt(&self) -> PieceType {
        self.flag().promotion_piece_type()
    }

    #[inline(always)]
    pub const fn is_quiet(&self) -> bool {
        self.flag() as u8 == MoveFlag::QuietMove as u8
    }

    #[inline(always)]
    pub const fn is_double_push(&self) -> bool {
        self.flag() as u8 == MoveFlag::DoublePawnPush as u8
    }

    #[inline(always)]
    pub const fn is_ep_capture(&self) -> bool {
        self.flag() as u8 == MoveFlag::EPCapture as u8
    }

    #[inline(always)]
    pub const fn is_king_castle(&self) -> bool {
        self.flag() as u8 == MoveFlag::KingCastle as u8
    }

    #[inline(always)]
    pub const fn is_queen_castle(&self) -> bool {
        self.flag() as u8 == MoveFlag::QueenCastle as u8
    }

    #[inline(always)]
    pub const fn is_castle(&self) -> bool {
        self.is_king_castle() || self.is_queen_castle()
    }

    #[inline(always)]
    pub const fn is_none(&self) -> bool {
        self.data == Self::NONE.data
    }

    #[inline(always)]
    pub const fn is_null(&self) -> bool {
        self.data == Self::NULL.data
    }

    #[inline(always)]
    pub const fn raw(&self) -> u16 {
        self.data
    }

    #[inline(always)]
    pub const fn is_valid(&self) -> bool {
        !self.is_none() && !self.is_null()
    }

    pub fn to_str(&self, board: &Board) -> String {
        if self.is_null() {
            "null".to_string()
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
            format!("{}{}{}", self.from(), self.to(), self.promotion_pt())
        } else {
            format!("{}{}", self.from(), self.to())
        }
    }
}

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
        assert!(m.is_capture());
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
        Move::new_promotion(A7, A8, PieceType::Pawn, false);
    }

    #[test]
    #[should_panic(expected = "Invalid promotion piece type!")]
    fn test_invalid_promotion_panic_king() {
        Move::new_promotion(A7, A8, PieceType::King, false);
    }

    #[test]
    fn test_query_methods_comprehensive() {
        let quiet = Move::new(G1, F3, MoveFlag::QuietMove);
        let capture = Move::new(E4, D5, MoveFlag::Capture);
        let ep = Move::new(E5, D6, MoveFlag::EPCapture);
        let promo_cap = Move::new_promotion(B7, C8, PieceType::Queen, true);
        let promo_quiet = Move::new_promotion(A7, A8, PieceType::Rook, false);
        let ks_castle = Move::new(E1, G1, MoveFlag::KingCastle);
        let qs_castle = Move::new(E8, C8, MoveFlag::QueenCastle);
        let dpp = Move::new(A2, A4, MoveFlag::DoublePawnPush);

        assert!(quiet.is_quiet());
        assert!(!capture.is_quiet());
        assert!(!ep.is_quiet());
        assert!(!promo_cap.is_quiet());
        assert!(!promo_quiet.is_quiet());
        assert!(!ks_castle.is_quiet());
        assert!(!qs_castle.is_quiet());
        assert!(!dpp.is_quiet());

        assert!(!quiet.is_capture());
        assert!(capture.is_capture());
        assert!(ep.is_capture());
        assert!(promo_cap.is_capture());
        assert!(!promo_quiet.is_capture());
        assert!(!ks_castle.is_capture());
        assert!(!qs_castle.is_capture());
        assert!(!dpp.is_capture());

        assert!(!quiet.is_promotion());
        assert!(!capture.is_promotion());
        assert!(!ep.is_promotion());
        assert!(promo_cap.is_promotion());
        assert!(promo_quiet.is_promotion());
        assert!(!ks_castle.is_promotion());
        assert!(!qs_castle.is_promotion());
        assert!(!dpp.is_promotion());

        assert_eq!(promo_cap.promotion_pt(), PieceType::Queen);
        assert_eq!(promo_quiet.promotion_pt(), PieceType::Rook);

        assert!(!quiet.is_ep_capture());
        assert!(!capture.is_ep_capture());
        assert!(ep.is_ep_capture());
        assert!(!promo_cap.is_ep_capture());

        assert!(!quiet.is_castle());
        assert!(ks_castle.is_king_castle());
        assert!(ks_castle.is_castle());
        assert!(!ks_castle.is_queen_castle());
        assert!(qs_castle.is_queen_castle());
        assert!(qs_castle.is_castle());
        assert!(!qs_castle.is_king_castle());

        assert!(!quiet.is_double_push());
        assert!(dpp.is_double_push());
        assert!(!capture.is_double_push());
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
        assert!(default_move.is_quiet());
    }
}
