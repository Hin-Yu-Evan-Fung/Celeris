use super::*;
use crate::board::Board;
use crate::core::*;
use moves::{Move, MoveFlag};

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
    let us = board.side_to_move();
    let them = !us;

    let enemy_bb = board.occupied_bb(them);
    let empty_bb = !board.all_occupied_bb();

    (dest & enemy_bb).for_each(|to| {
        move_list.add_move(Move::new(from, to, MoveFlag::Capture));
    });

    if G::gen_type() != MoveGenType::Capture {
        (dest & empty_bb).for_each(|to| {
            move_list.add_move(Move::new(from, to, MoveFlag::QuietMove));
        })
    }
}

#[inline]
fn add_castling_move(index: usize, board: &Board, move_list: &mut MoveList) {
    let castling = [Castling::WK, Castling::WQ, Castling::BK, Castling::BQ][index];

    if board.castling().has(castling) {
        let ksq = board.ksq(board.side_to_move());
        let dest = [Square::G1, Square::C1, Square::G8, Square::C8][index];
        let rook_dest = [Square::F1, Square::D1, Square::F8, Square::D8][index];
        let flag = [MoveFlag::KingCastle, MoveFlag::QueenCastle][index & 0b1];

        let rook_sq = unsafe { board.rook_sq(index) };

        let king_path = pin_bb(ksq, dest);
        let move_area = king_path | pin_bb(rook_sq, rook_dest);

        let occ = board.all_occupied_bb() ^ rook_sq.bb() ^ ksq.bb();

        if (king_path & board.attacked()).is_empty() && (move_area & occ).is_empty() {
            move_list.add_move(Move::new(ksq, dest, flag));
        }
    }
}

/******************************************\
|==========================================|
|              Move Generation             |
|==========================================|
\******************************************/

pub fn generate_move<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    if board.check_mask() == Bitboard::EMPTY {
        generate_king_moves::<G>(board, move_list);
    } else {
        generate_pawn_moves::<G>(board, move_list);
        generate_knight_moves::<G>(board, move_list);
        generate_bishop_moves::<G>(board, move_list);
        generate_rook_moves::<G>(board, move_list);
        generate_queen_moves::<G>(board, move_list);
        generate_king_moves::<G>(board, move_list);
        if board.check_mask() == Bitboard::FULL && G::gen_type() != MoveGenType::Capture {
            generate_castling_moves(board, move_list);
        }
    }
}

fn generate_pawn_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    match G::gen_type() {
        MoveGenType::Quiet => generate_pawn_quiets(board, move_list),
        MoveGenType::Capture => generate_pawn_captures(board, move_list),
        MoveGenType::Legal => {
            generate_pawn_captures(board, move_list);
            generate_pawn_quiets(board, move_list);
        }
        _ => unreachable!(),
    }
}

fn generate_pawn_quiets(board: &Board, move_list: &mut MoveList) {
    let us = board.side_to_move();

    // Push and double push directions
    let push = us.forward();
    let double_push = us.forward_double();

    // Non occupied bb (Empty squares on the board)
    let empty_bb = !board.all_occupied_bb();
    // Push rank
    let push_rank = Bitboard::push_rank(us);
    // Promotion rank
    let promo_rank = Bitboard::promo_rank(us);
    // Diagonal pin mask
    let diag_pin = board.diag_pin();
    // Horizontal/Vertical pin mask
    let hv_pin = board.hv_pin();
    // Check mask
    let check_mask = board.check_mask();

    // Get pawns that can move vertically
    let moveable = board.piece_bb(us, PieceType::Pawn) & !diag_pin;
    // Pawns that can move forward (pseudo legal, rook pin checked later)
    let mut pushable = moveable & empty_bb.shift(-push);
    // Pawns that can move forward two squares
    let double_pushable = pushable // Pawns that can move forward
        & (empty_bb & check_mask).shift(-double_push) // Filter the pawns that can be pushed and blocks check
        & push_rank // Filter the pawns that are on the push rank (Rank 2 or Rank 7 )
        & (!hv_pin | hv_pin.shift(-double_push)); // Filter the pawns that are pinned and cant move

    // Filter out the pawns that block checks and are not pinned
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

fn generate_pawn_captures(board: &Board, move_list: &mut MoveList) {
    let us = board.side_to_move();
    let them = !us;

    // Push and double push directions
    let left = us.forward_left();
    let right = us.forward_right();
    let ep_left = if us == Colour::White {
        Direction::W
    } else {
        Direction::E
    };
    let ep_right = -ep_left;

    // Non occupied bb (Empty squares on the board)
    let enemy_bb = board.occupied_bb(them);
    // Promotion rank
    let promo_rank = Bitboard::promo_rank(us);
    // Diagonal pin mask
    let diag_pin = board.diag_pin();
    // Horizontal/Vertical pin mask
    let hv_pin = board.hv_pin();
    // Check mask
    let check_mask = board.check_mask();

    // Get pawns that can move diagonally
    let movable = board.piece_bb(us, PieceType::Pawn) & !hv_pin;
    // Get pawns taht can attack left
    let pawn_left =
        movable & (enemy_bb & check_mask).shift(-left) & (!diag_pin | diag_pin.shift(-left));
    // Get pawns that can attack right
    let pawn_right =
        movable & (enemy_bb & check_mask).shift(-right) & (!diag_pin | diag_pin.shift(-right));

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

fn generate_knight_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.side_to_move();

    let check_mask = board.check_mask();
    let diag_pin = board.diag_pin();
    let hv_pin = board.hv_pin();

    let knights = board.piece_bb(us, PieceType::Knight) & !(diag_pin | hv_pin);

    knights.for_each(|from| {
        let dest = leaper_attack(PieceType::Knight, from) & check_mask;
        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

fn generate_bishop_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.side_to_move();

    let check_mask = board.check_mask();
    let diag_pin = board.diag_pin();
    let hv_pin = board.hv_pin();
    let all_occ = board.all_occupied_bb();

    let queens = board.piece_bb(us, PieceType::Queen);

    let bishops = board.piece_bb(us, PieceType::Bishop) & !hv_pin;

    let pinned = (bishops | queens) & diag_pin;

    let non_pinned = bishops & !diag_pin;

    pinned.for_each(|from| {
        let dest = slider_attack(PieceType::Bishop, from, all_occ) & check_mask & diag_pin;
        add_piece_moves::<G>(board, from, dest, move_list);
    });

    non_pinned.for_each(|from| {
        let dest = slider_attack(PieceType::Bishop, from, all_occ) & check_mask;
        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

fn generate_rook_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.side_to_move();

    let check_mask = board.check_mask();
    let diag_pin = board.diag_pin();
    let hv_pin = board.hv_pin();
    let all_occ = board.all_occupied_bb();

    let queens = board.piece_bb(us, PieceType::Queen);

    let rooks = board.piece_bb(us, PieceType::Rook) & !diag_pin;

    let pinned = (rooks | queens) & hv_pin;

    let non_pinned = rooks & !hv_pin;

    pinned.for_each(|from| {
        let dest = slider_attack(PieceType::Rook, from, all_occ) & check_mask & hv_pin;
        add_piece_moves::<G>(board, from, dest, move_list);
    });

    non_pinned.for_each(|from| {
        let dest = slider_attack(PieceType::Rook, from, all_occ) & check_mask;
        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

fn generate_queen_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.side_to_move();

    let check_mask = board.check_mask();
    let diag_pin = board.diag_pin();
    let hv_pin = board.hv_pin();
    let all_occ = board.all_occupied_bb();

    let queens = board.piece_bb(us, PieceType::Queen);

    let non_pinned = queens & !(diag_pin | hv_pin);

    non_pinned.for_each(|from| {
        let dest = slider_attack(PieceType::Queen, from, all_occ) & check_mask;
        add_piece_moves::<G>(board, from, dest, move_list);
    });
}

fn generate_king_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.side_to_move();

    let from = board.ksq(us);
    let dest = leaper_attack(PieceType::King, from) & !board.attacked();
    add_piece_moves::<G>(board, from, dest, move_list);
}

fn generate_castling_moves(board: &Board, move_list: &mut MoveList) {
    let us = board.side_to_move();

    match us {
        Colour::White => {
            add_castling_move(0, board, move_list);
            add_castling_move(1, board, move_list);
        }
        Colour::Black => {
            add_castling_move(2, board, move_list);
            add_castling_move(3, board, move_list);
        }
    }
}
