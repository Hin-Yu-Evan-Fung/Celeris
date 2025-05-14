use chess::{
    Bitboard, Colour, Move, Piece, PieceType, Square,
    board::Board,
    board::{bishop_attacks, rook_attacks},
};

use crate::Eval;

const VALUES: [Eval; PieceType::NUM] = [
    Eval(150),
    Eval(340),
    Eval(360),
    Eval(480),
    Eval(1000),
    Eval(0),
];

fn move_value(board: &Board, move_: Move) -> Eval {
    let (from, to) = (move_.from(), move_.to());

    let moved_pt = unsafe { board.on_unchecked(from).pt().index() };

    let mut value = Eval::ZERO;

    if move_.is_capture() {
        if move_.is_ep_capture() {
            return VALUES[PieceType::Pawn.index()];
        } else {
            value += unsafe { VALUES[board.on_unchecked(to).pt().index()] };
        }
    }

    if move_.is_promotion() {
        value += -VALUES[moved_pt] + VALUES[move_.promotion_pt().index()];
    }

    value
}

fn lva(board: &Board, attackers: Bitboard, us: Colour) -> (Square, Piece) {
    for pt in PieceType::iter() {
        let bb = attackers & board.piece_bb(us, pt);

        if bb.is_occupied() {
            let square = bb.lsb_unchecked();
            return (square, Piece::from_parts(us, pt));
        }
    }

    unreachable!()
}

pub fn see(board: &Board, move_: Move, threshold: Eval) -> bool {
    use PieceType::*;

    let (from, to) = (move_.from(), move_.to());

    if move_.is_castle() {
        return true;
    }

    let mut gain = move_value(board, move_) - threshold;

    if gain < Eval::ZERO {
        return false;
    }

    let victim = if move_.is_promotion() {
        move_.promotion_pt().index()
    } else {
        unsafe { board.on_unchecked(from).pt().index() }
    };

    gain -= VALUES[victim];

    if gain >= Eval::ZERO {
        return true;
    }

    let diag_sliders = board.piecetype_bb(Bishop) | board.piecetype_bb(Queen);
    let hv_sliders = board.piecetype_bb(Rook) | board.piecetype_bb(Queen);

    let mut occ = board.all_occupied_bb();

    occ = (occ ^ from.bb()) | to.bb();
    if move_.is_ep_capture() {
        occ ^= board.ep_target().unwrap().bb();
    }

    let mut attackers = board.attackers_to(to, occ) & occ;

    let mut stm = !board.stm();

    loop {
        let stm_attackers = attackers & board.occupied_bb(stm);

        if stm_attackers.is_empty() {
            break;
        }

        let (lva_sq, lva) = lva(board, stm_attackers, stm);

        occ.clear(lva_sq);

        let lva_pt = lva.pt();

        if matches!(lva_pt, Pawn | Bishop | Queen) {
            attackers |= bishop_attacks(to, occ) & diag_sliders;
        }

        if matches!(lva_pt, Rook | Queen) {
            attackers |= rook_attacks(to, occ) & hv_sliders;
        }

        attackers &= occ;

        stm = !stm;

        gain = -gain - Eval(1) - VALUES[lva_pt.index()];

        if gain >= Eval::ZERO {
            if lva_pt == King && (attackers & board.occupied_bb(stm)).is_occupied() {
                stm = !stm;
            }
            break;
        }
    }

    stm != board.stm()
}

#[cfg(test)]
mod tests {
    use super::*;
    use chess::{
        Move, MoveFlag, Square,
        board::Board,
        board::{LegalGen, MoveList},
    };

    #[test]
    fn test_see_simple_winning_capture() {
        let board = Board::from_fen("r1k5/8/8/8/8/8/8/Q6K w - - 0 1").unwrap();
        let move_ = Move::new(Square::A1, Square::A8, MoveFlag::Capture);
        assert!(see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_simple_losing_capture() {
        let board = Board::from_fen("2q5/1k1p4/8/8/Q7/8/8/7K w - - 0 1").unwrap();
        let move_ = Move::new(Square::A4, Square::D7, MoveFlag::Capture);
        assert!(!see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_equal_exchange() {
        let board = Board::from_fen("r6k/8/8/8/8/8/8/R6K w - - 0 1").unwrap();
        let move_ = Move::new(Square::A1, Square::A8, MoveFlag::Capture);

        assert!(see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_complex_exchange_winning() {
        let board = Board::from_fen("1k1r4/1ppn2b1/p7/4pp2/P3P1q1/1P1N4/2P1QPPP/R2B1RK1 w - - 0 1")
            .unwrap();
        let move_ = Move::new(Square::E2, Square::G4, MoveFlag::Capture);
        assert!(see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_complex_exchange_losing() {
        let board =
            Board::from_fen("rnbqkb1r/ppp1pppp/5n2/3p4/3P1B2/2N5/PPP1PPPP/R2QKBNR w KQkq - 0 1")
                .unwrap();
        let move_ = Move::new(Square::C3, Square::D5, MoveFlag::Capture);
        assert!(!see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_en_passant() {
        let board = Board::from_fen("4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1").unwrap();
        let move_ = Move::new(Square::E5, Square::D6, MoveFlag::EPCapture);

        assert!(see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_promotion_capture() {
        let board = Board::from_fen("r7/1P6/k7/8/8/8/8/K7 w - - 0 1").unwrap();
        let move_ = Move::new(Square::B7, Square::A8, MoveFlag::QueenPromo);

        assert!(see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_underpromotion_capture() {
        let board = Board::from_fen("r7/1P6/k7/8/8/8/8/K7 w - - 0 1").unwrap();
        let move_ = Move::new(Square::B7, Square::A8, MoveFlag::KnightPromo);

        assert!(see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_king_involved_losing() {
        let board = Board::from_fen("8/8/8/4k3/3p4/8/8/3QK3 w - - 0 1").unwrap();
        let move_ = Move::new(Square::D1, Square::D4, MoveFlag::Capture);
        assert!(!see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_castling() {
        let board = Board::from_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1").unwrap();
        let move_ = Move::new(Square::E1, Square::G1, MoveFlag::KingCastle);
        assert!(see(&board, move_, Eval::ZERO));
    }

    #[test]
    fn test_see_bulk() {
        const P: i16 = 150;
        const N: i16 = 340;
        const R: i16 = 480;
        const Q: i16 = 1000;

        #[rustfmt::skip]
        const SEE_TESTS: &[(&str, &str, i16, bool)] = &[
            ("2k5/8/8/4p3/8/8/2K1R3/8 w - - 0 1", "e2e5", 0, true),
            ("3k4/8/8/4p3/3P4/8/8/5K2 w - - 0 1", "d4e5", P, true),
            ("3k4/8/5p2/4p3/3P4/8/8/5K2 w - - 0 1", "d4e5", P, false),
            ("8/3k4/2n2b2/8/3P4/8/3KN3/8 b - - 0 1", "c6d4", P, true),
            ("8/3k4/2n2b2/8/3P4/8/3KN3/8 b - - 0 1", "c6d4", N, false),
            ("3kr3/8/4q3/8/4P3/5P2/8/3K4 b - - 0 1", "e6e4", 0, false),
            ("3kr3/8/4q3/8/4P3/5P2/8/3K4 b - - 0 1", "e6e4", -Q, true),
            ("8/3k4/2n2b2/8/3P4/3K4/4N3/8 b - - 0 1", "c6d4", P, false),
            ("5k2/2P5/4b3/8/8/8/8/2R2K2 w - - 0 1", "c7c8q", 0, true),
            ("5k2/2P5/4b3/8/8/8/8/3R1K2 w - - 0 1", "c7c8q", 0, false),
            ("8/3k2b1/2n2b2/8/3P4/3K4/4N3/8 b - - 0 1", "c6d4", 0, true),
            ("3k4/8/2q5/2b5/2r5/8/2P5/2R1K3 b - - 0 1", "c4c2", 0, false),
            ("3k4/8/2q5/2b5/2r5/8/2P5/2R1K3 b - - 0 1", "c4c2", P - R, true),
            ("2k5/3n2b1/2nq4/4R3/5P2/3N1N2/8/5K2 b - - 0 1", "d6e5", 0, false),
            ("2k5/3n2b1/2nq4/4R3/5P2/3N1N2/8/5K2 b - - 0 1", "d6e5", R - Q + P, true),
            ("5r1k/3b1q1p/1npb4/1p6/pPpP1N2/2P4B/2NBQ1P1/5R1K b - - 0 1", "d6f4", 0, false),
            ("5r1k/3b1q1p/1npb4/1p6/pPpP1N2/2P4B/2NBQ1P1/5R1K b - - 0 1", "d6f4", -P, true),
        ];

        for (fen, move_str, threshold, result) in SEE_TESTS {
            let board: Board = Board::from_fen(fen).unwrap();

            let mut move_list = MoveList::new();
            board.generate_moves::<LegalGen>(&mut move_list);

            let move_ = *move_list
                .iter()
                .find(|&move_| move_.to_str(&board) == *move_str)
                .unwrap();

            println!("{}", board.fen());
            assert_eq!(see(&board, move_, Eval(*threshold)), *result);
        }
    }
}
