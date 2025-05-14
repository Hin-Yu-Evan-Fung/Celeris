use super::*;
use crate::board::Board;
use crate::core::*;
use crate::{Move, MoveFlag};

/******************************************\
|==========================================|
|              Helper Functions            |
|==========================================|
\******************************************/

#[inline]
fn add_promo_quiets(from: Square, to: Square, move_list: &mut MoveList) {
    move_list.add_move(Move::new(from, to, MoveFlag::QueenPromo));
    move_list.add_move(Move::new(from, to, MoveFlag::RookPromo));
    move_list.add_move(Move::new(from, to, MoveFlag::BishopPromo));
    move_list.add_move(Move::new(from, to, MoveFlag::KnightPromo));
}

#[inline]
fn add_promo_captures(from: Square, to: Square, move_list: &mut MoveList) {
    move_list.add_move(Move::new(from, to, MoveFlag::QueenPromoCapture));
    move_list.add_move(Move::new(from, to, MoveFlag::RookPromoCapture));
    move_list.add_move(Move::new(from, to, MoveFlag::BishopPromoCapture));
    move_list.add_move(Move::new(from, to, MoveFlag::KnightPromoCapture));
}

#[inline]
fn add_promo_moves<const CAPTURE: bool>(bb: Bitboard, move_list: &mut MoveList, dir: Direction) {
    bb.for_each(|from| {
        let to = unsafe { from.add_unchecked(dir) };
        if CAPTURE {
            add_promo_captures(from, to, move_list);
        } else {
            add_promo_quiets(from, to, move_list);
        }
    });
}

#[inline]
fn add_pawn_moves(bb: Bitboard, move_list: &mut MoveList, dir: Direction, flag: MoveFlag) {
    bb.for_each(|from| {
        let to = unsafe { from.add_unchecked(dir) };
        move_list.add_move(Move::new(from, to, flag));
    })
}

#[inline]
fn add_piece_moves<G: GenTypeTrait>(
    board: &Board,
    from: Square,
    dest: Bitboard,
    move_list: &mut MoveList,
) {
    let us = board.stm();
    let them = !us;

    let enemy_bb = board.occupied_bb(them);
    let empty_bb = !board.all_occupied_bb();

    if G::gen_type() != MoveGenType::Quiet {
        (dest & enemy_bb).for_each(|to| {
            move_list.add_move(Move::new(from, to, MoveFlag::Capture));
        });
    }

    if G::gen_type() != MoveGenType::Capture {
        (dest & empty_bb).for_each(|to| {
            move_list.add_move(Move::new(from, to, MoveFlag::QuietMove));
        })
    }
}

#[inline]
fn add_castling_move(castle: Castling, board: &Board, move_list: &mut MoveList) {
    debug_assert!(
        castle.0.count_ones() == 1,
        "This function only works for castling on one side (atomic)"
    );

    if board.castling().has(castle) && board.can_castle(castle) {
        let ksq = board.ksq(board.stm());

        let dest = board.castling_king_dest(castle);

        let flag = board.castling_flag(castle);

        move_list.add_move(Move::new(ksq, dest, flag));
    }
}

/******************************************\
|==========================================|
|              Move Generation             |
|==========================================|
\******************************************/

pub(crate) fn generate_move<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    if board.check_mask() == Bitboard::EMPTY {
        gen_king_moves::<G>(board, move_list);
    } else {
        gen_pawn_moves::<G>(board, move_list);
        gen_knight_moves::<G>(board, move_list);
        gen_diag_slider_moves::<G>(board, move_list);
        gen_hv_slider_moves::<G>(board, move_list);
        gen_king_moves::<G>(board, move_list);

        if board.check_mask() == Bitboard::FULL && G::gen_type() != MoveGenType::Capture {
            gen_castling_moves(board, move_list);
        }
    }
}

fn gen_pawn_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    match G::gen_type() {
        MoveGenType::Quiet => gen_pawn_quiets(board, move_list),
        MoveGenType::Capture => gen_pawn_captures(board, move_list),
        MoveGenType::Legal => {
            gen_pawn_captures(board, move_list);
            gen_pawn_quiets(board, move_list);
        }
    }
}

fn gen_pawn_quiets(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    let push = us.forward();
    let double_push = us.double_forward();
    let push_rank = Bitboard::push_rank(us);
    let promo_rank = Bitboard::promo_rank(us);

    let empty_bb = !board.all_occupied_bb();
    let diag_pin = board.diag_pin();
    let hv_pin = board.hv_pin();
    let check_mask = board.check_mask();

    let moveable = board.piece_bb(us, PieceType::Pawn) & !diag_pin;

    let mut pushable = moveable & empty_bb.shift(-push);

    let double_pushable = pushable
        & (empty_bb & check_mask).shift(-double_push)
        & push_rank
        & (!hv_pin | hv_pin.shift(-double_push));

    pushable &= check_mask.shift(-push) & (!hv_pin | hv_pin.shift(-push));

    if (pushable & promo_rank).is_occupied() {
        add_promo_moves::<false>(pushable & promo_rank, move_list, push);
        add_pawn_moves(pushable & !promo_rank, move_list, push, MoveFlag::QuietMove);
    } else {
        add_pawn_moves(pushable, move_list, push, MoveFlag::QuietMove);
    }

    add_pawn_moves(
        double_pushable,
        move_list,
        double_push,
        MoveFlag::DoublePawnPush,
    );
}

fn gen_pawn_captures(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();
    let them = !us;

    let left = us.forward_left();
    let right = us.forward_right();
    let ep_left = if us == Colour::White {
        Direction::W
    } else {
        Direction::E
    };
    let ep_right = -ep_left;

    let enemy_bb = board.occupied_bb(them);
    let promo_rank = Bitboard::promo_rank(us);
    let diag_pin = board.diag_pin();
    let hv_pin = board.hv_pin();
    let check_mask = board.check_mask();

    let movable = board.piece_bb(us, PieceType::Pawn) & !hv_pin;

    let mut pawn_left = movable & (enemy_bb & check_mask).shift(-left);

    pawn_left &= !diag_pin | diag_pin.shift(-left);

    let mut pawn_right = movable & (enemy_bb & check_mask).shift(-right);

    pawn_right &= !diag_pin | diag_pin.shift(-right);

    if let Some(ep_sq) = board.ep() {
        if !board.ep_pin() {
            let ep_target = unsafe { ep_sq.add_unchecked(-us.forward()) };

            let ep_left = movable
                & (ep_target.bb() & check_mask).shift(ep_left)
                & (!diag_pin | diag_pin.shift(-left));
            let ep_right = movable
                & (ep_target.bb() & check_mask).shift(ep_right)
                & (!diag_pin | diag_pin.shift(-right));

            if ep_left.is_occupied() {
                let from = unsafe { ep_sq.add_unchecked(-left) };
                move_list.add_move(Move::new(from, ep_sq, MoveFlag::EPCapture));
            }

            if ep_right.is_occupied() {
                let from = unsafe { ep_sq.add_unchecked(-right) };
                move_list.add_move(Move::new(from, ep_sq, MoveFlag::EPCapture));
            }
        }
    }

    if ((pawn_left | pawn_right) & promo_rank).is_occupied() {
        add_promo_moves::<true>(pawn_left & promo_rank, move_list, left);
        add_promo_moves::<true>(pawn_right & promo_rank, move_list, right);
        add_pawn_moves(pawn_left & !promo_rank, move_list, left, MoveFlag::Capture);
        add_pawn_moves(
            pawn_right & !promo_rank,
            move_list,
            right,
            MoveFlag::Capture,
        );
    } else {
        add_pawn_moves(pawn_left, move_list, left, MoveFlag::Capture);
        add_pawn_moves(pawn_right, move_list, right, MoveFlag::Capture);
    }
}

fn gen_knight_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    let check_mask = board.check_mask();
    let pin_mask = board.diag_pin() | board.hv_pin();

    let knights = board.piece_bb(us, PieceType::Knight) & !pin_mask;

    knights.for_each(|from| {
        let dest = knight_attack(from) & check_mask;

        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

fn gen_diag_slider_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    let check_mask = board.check_mask();
    let diag_pin = board.diag_pin();
    let hv_pin = board.hv_pin();
    let all_occ = board.all_occupied_bb();

    let pieces =
        (board.piece_bb(us, PieceType::Queen) | board.piece_bb(us, PieceType::Bishop)) & !hv_pin;

    let pinned = pieces & diag_pin;
    let non_pinned = pieces & !diag_pin;

    pinned.for_each(|from| {
        let dest = bishop_attacks(from, all_occ) & check_mask & diag_pin;
        add_piece_moves::<G>(board, from, dest, move_list);
    });

    non_pinned.for_each(|from| {
        let dest = bishop_attacks(from, all_occ) & check_mask;
        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

fn gen_hv_slider_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    let check_mask = board.check_mask();
    let diag_pin = board.diag_pin();
    let hv_pin = board.hv_pin();
    let all_occ = board.all_occupied_bb();

    let pieces =
        (board.piece_bb(us, PieceType::Queen) | board.piece_bb(us, PieceType::Rook)) & !diag_pin;

    let pinned = pieces & hv_pin;
    let non_pinned = pieces & !hv_pin;

    pinned.for_each(|from| {
        let dest = rook_attacks(from, all_occ) & check_mask & hv_pin;
        add_piece_moves::<G>(board, from, dest, move_list);
    });

    non_pinned.for_each(|from| {
        let dest = rook_attacks(from, all_occ) & check_mask;
        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

fn gen_king_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    let from = board.ksq(us);

    let dest = king_attack(from) & !board.attacked();

    add_piece_moves::<G>(board, from, dest, move_list);
}

fn gen_castling_moves(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    match us {
        Colour::White => {
            add_castling_move(Castling::WK, board, move_list);
            add_castling_move(Castling::WQ, board, move_list);
        }
        Colour::Black => {
            add_castling_move(Castling::BK, board, move_list);
            add_castling_move(Castling::BQ, board, move_list);
        }
    }
}
