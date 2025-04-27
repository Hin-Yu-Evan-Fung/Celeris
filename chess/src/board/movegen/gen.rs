//! # Module: `move_gen`
//!
//! This module is responsible for generating legal chess moves for a given board state.
//! It utilizes precomputed lookup tables (from the `lookup` module) and bitboard manipulation
//! for high performance. The generation process considers pins, checks, special moves like
//! castling and en passant, and promotions.
//!
//! ## Core Concepts
//!
//! - **Bitboards**: Used extensively to represent piece locations, attack sets, and masks.
//! - **Precomputed Lookups**: Relies on tables for piece attacks (`leaper_attack`, `slider_attack`),
//!   lines between squares (`line_bb`, `between_bb`), pin masks (`pin_bb`), and check masks (`check_bb`).
//! - **Pins and Checks**: Move generation correctly handles absolute pins and checks. Pinned pieces
//!   have restricted movement, and when in check, only moves that resolve the check (blocking,
//!   capturing the checker, or moving the king) are generated. Double checks restrict moves to only
//!   king moves.
//! - **`GenTypeTrait`**: A generic parameter used throughout the module to specify the type of moves
//!   to generate:
//!     - `Legal`: Generate all legal moves.
//!     - `Capture`: Generate only legal capture moves (including en passant and promotion captures).
//!     - `Quiet`: Generate only legal non-capture moves (including castling and quiet promotions).
//!   This allows optimizing move generation for specific search contexts (e.g., quiescence search).
//! - **`MoveList`**: A data structure (likely defined elsewhere) used to store the generated moves.
//!
//! ## Main Entry Point
//!
//! The primary function is [`generate_move`], which orchestrates the generation process based on the
//! board state and the requested `GenType`.
//!
//! ## Structure
//!
//! The module contains:
//! - Helper functions (`add_promo_quiets`, `add_promo_captures`, etc.) to simplify adding moves
//!   with specific flags to the `MoveList`.
//! - Piece-specific generator functions (`gen_pawn_moves`, `gen_knight_moves`, etc.) that implement
//!   the logic for generating moves for each piece type, considering legality rules.
//! - The main [`generate_move`] function that calls the appropriate piece generators based on
//!   the check state and `GenType`.
use super::*;
use crate::board::Board;
use crate::core::*;
use moves::{Move, MoveFlag};

/******************************************\
|==========================================|
|              Helper Functions            |
|==========================================|
\******************************************/

/// Adds all four quiet promotion moves (Queen, Rook, Bishop, Knight) for a pawn move.
///
/// # Arguments
/// * `from` - The starting square of the pawn.
/// * `to` - The destination square (on the promotion rank).
/// * `move_list` - The list to add the generated moves to.
#[inline]
fn add_promo_quiets(from: Square, to: Square, move_list: &mut MoveList) {
    move_list.add_move(Move::new(from, to, MoveFlag::QueenPromo));
    move_list.add_move(Move::new(from, to, MoveFlag::RookPromo));
    move_list.add_move(Move::new(from, to, MoveFlag::BishopPromo));
    move_list.add_move(Move::new(from, to, MoveFlag::KnightPromo));
}

/// Adds all four capture promotion moves (Queen, Rook, Bishop, Knight) for a pawn capture.
///
/// # Arguments
/// * `from` - The starting square of the pawn.
/// * `to` - The destination square (on the promotion rank).
/// * `move_list` - The list to add the generated moves to.
#[inline]
fn add_promo_captures(from: Square, to: Square, move_list: &mut MoveList) {
    move_list.add_move(Move::new(from, to, MoveFlag::QueenPromoCapture));
    move_list.add_move(Move::new(from, to, MoveFlag::RookPromoCapture));
    move_list.add_move(Move::new(from, to, MoveFlag::BishopPromoCapture));
    move_list.add_move(Move::new(from, to, MoveFlag::KnightPromoCapture));
}

/// Adds promotion moves (quiet or capture) for a set of pawns reaching the promotion rank.
///
/// Iterates through the `bb` bitboard. For each set bit (`from` square), calculates the `to`
/// square based on the `dir`, and adds the appropriate set of four promotion moves
/// (quiet or capture based on `CAPTURE` const generic) to the `move_list`.
///
/// # Arguments
/// * `CAPTURE` - Const generic bool indicating if the promotions are captures (`true`) or quiet (`false`).
/// * `bb` - A bitboard of pawns on the rank *before* the promotion rank, ready to move one step.
/// * `move_list` - The list to add the generated moves to.
/// * `dir` - The direction the pawns are moving (typically `N` for White, `S` for Black).
#[inline]
fn add_promo_moves<const CAPTURE: bool>(bb: Bitboard, move_list: &mut MoveList, dir: Direction) {
    bb.for_each(|from| {
        // Safety: `from` is guaranteed to be on the 7th/2nd rank, so adding N/S is safe.
        let to = unsafe { from.add_unchecked(dir) };
        if CAPTURE {
            add_promo_captures(from, to, move_list);
        } else {
            add_promo_quiets(from, to, move_list);
        }
    });
}

/// Adds standard (non-promotion, non-en-passant) pawn moves for a set of pawns.
///
/// Iterates through the `bb` bitboard. For each set bit (`from` square), calculates the `to`
/// square based on the `dir`, and adds a single move with the specified `flag` to the `move_list`.
///
/// # Arguments
/// * `bb` - A bitboard of pawns making the same type of move (e.g., single push, double push, capture).
/// * `move_list` - The list to add the generated moves to.
/// * `dir` - The direction the pawns are moving.
/// * `flag` - The `MoveFlag` for this type of move (e.g., `QuietMove`, `DoublePawnPush`, `Capture`).
#[inline]
fn add_pawn_moves(bb: Bitboard, move_list: &mut MoveList, dir: Direction, flag: MoveFlag) {
    bb.for_each(|from| {
        // Safety: The calling context ensures `from` + `dir` is a valid destination square.
        let to = unsafe { from.add_unchecked(dir) };
        move_list.add_move(Move::new(from, to, flag));
    })
}

/// Adds moves for a single piece (Knight, Bishop, Rook, Queen, King) from a given square.
///
/// Takes the set of potential destination squares (`dest`), filters them into captures
/// (squares occupied by the enemy) and quiet moves (empty squares), and adds them
/// to the `move_list` according to the `GenTypeTrait` `G`.
///
/// # Arguments
/// * `G` - The `GenTypeTrait` specifying whether to generate captures, quiets, or both.
/// * `board` - The current board state.
/// * `from` - The square the piece is moving from.
/// * `dest` - A bitboard of pseudo-legal destination squares for the piece.
/// * `move_list` - The list to add the generated moves to.
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

    // Add captures (always generated unless G is specifically Quiet, but filtering happens later if needed)
    (dest & enemy_bb).for_each(|to| {
        move_list.add_move(Move::new(from, to, MoveFlag::Capture));
    });

    // Add quiet moves only if G is not Capture-only
    if G::gen_type() != MoveGenType::Capture {
        (dest & empty_bb).for_each(|to| {
            move_list.add_move(Move::new(from, to, MoveFlag::QuietMove));
        })
    }
}

/// Checks legality and adds a specific castling move if valid.
///
/// # Arguments
/// * `index` - An index determining the castling type: 0=WK, 1=WQ, 2=BK, 3=BQ.
/// * `board` - The current board state.
/// * `move_list` - The list to add the castling move to if legal.
#[inline]
fn add_castling_move(castle: Castling, board: &Board, move_list: &mut MoveList) {
    debug_assert!(
        castle.0.count_ones() == 1,
        "This function only works for castling on one side (atomic)"
    );

    // Check if the side to move still has this castling right
    if board.castling().has(castle) && board.can_castle(castle) {
        let ksq = board.ksq(board.stm());

        let dest = board.castling_king_dest(castle);

        let flag = board.castling_flag(castle);

        // Add the castling move (e.g., E1 -> G1 with KingCastle flag)
        move_list.add_move(Move::new(ksq, dest, flag));
    }
}

/******************************************\
|==========================================|
|              Move Generation             |
|==========================================|
\******************************************/

/// Generates legal moves for the current board position.
///
/// This is the main entry point for move generation. It determines the check state
/// and calls the appropriate helper functions to generate moves for each piece type,
/// respecting pins, checks, and the requested `GenType`.
///
/// # Arguments
/// * `G` - The `GenTypeTrait` specifying whether to generate captures, quiets, or both.
/// * `board` - The current board state, containing piece positions, side to move,
///   castling rights, en passant square, pin masks, check mask, and attacked squares.
/// * `move_list` - The list to add the generated legal moves to.
///
/// # Logic Flow
/// 1. **Check State Determination**: Uses `board.check_mask()`:
///    - `Bitboard::EMPTY`: Indicates a double check (or potentially an impossible state). Only king moves are possible.
///    - `Bitboard::FULL`: Indicates the king is not in check. All piece moves are potentially valid, and castling might be possible.
///    - Other `Bitboard`: Indicates a single check. The `check_mask` contains the squares pieces can move to block the check or capture the checker. King moves are restricted to non-attacked squares.
/// 2. **Move Generation**:
///    - If **double check** (`EMPTY` mask): Only call `gen_king_moves`.
///    - If **single check or no check** (`!EMPTY` mask):
///        - Call generators for pawns, knights, bishops, rooks, and queens (`gen_pawn_moves`, `gen_knight_moves`, etc.). These functions internally use the `check_mask` and pin masks (`board.hv_pin()`, `board.diag_pin()`) to filter for legal moves.
///        - Call `gen_king_moves` (which uses `board.attacked()` for legality).
///        - If **not in check** (`FULL` mask) and `G` allows quiet moves: Call `gen_castling_moves`.
pub(crate) fn generate_move<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    // Assumption: check_mask == EMPTY means double check (only king moves)
    //             check_mask == FULL means no check
    //             otherwise, single check (mask defines blocking/capture squares)
    if board.check_mask() == Bitboard::EMPTY {
        // Double Check: Only king moves are possible.
        gen_king_moves::<G>(board, move_list);
    } else {
        // Not Double Check (Single Check or No Check): Generate moves for all pieces.
        // Generators internally handle pins and the check_mask.
        gen_pawn_moves::<G>(board, move_list);
        gen_knight_moves::<G>(board, move_list);
        gen_diag_slider_moves::<G>(board, move_list); // Bishops + Queen diagonal
        gen_hv_slider_moves::<G>(board, move_list); // Rooks + Queen rank/file
        gen_king_moves::<G>(board, move_list);

        // Castling is only possible if not in check and if generating quiet moves.
        if board.check_mask() == Bitboard::FULL && G::gen_type() != MoveGenType::Capture {
            gen_castling_moves(board, move_list);
        }
    }
}

/// Generates legal pawn moves (captures, pushes, promotions, en passant).
/// Calls specific quiet/capture generators based on `GenType`.
fn gen_pawn_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    match G::gen_type() {
        MoveGenType::Quiet => gen_pawn_quiets(board, move_list),
        MoveGenType::Capture => gen_pawn_captures(board, move_list),
        MoveGenType::Legal => {
            // Generate both captures and quiets when Legal is requested
            gen_pawn_captures(board, move_list);
            gen_pawn_quiets(board, move_list);
        }
    }
}

/// Generates quiet pawn moves (single pushes, double pushes, quiet promotions).
/// Considers pins and checks.
fn gen_pawn_quiets(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    // Directions and Ranks
    let push = us.forward();
    let double_push = us.forward_double();
    let push_rank = Bitboard::push_rank(us); // Starting rank for pawns (2 for white, 7 for black)
    let promo_rank = Bitboard::promo_rank(us); // Rank *before* promotion (7 for white, 2 for black)

    // Board state masks
    let empty_bb = !board.all_occupied_bb();
    let diag_pin = board.diag_pin(); // Pawns pinned diagonally cannot push
    let hv_pin = board.hv_pin(); // Pawns pinned vertically have restricted pushes
    let check_mask = board.check_mask(); // If checked, push must block check

    // Get pawns that can move vertically
    let moveable = board.piece_bb(us, PieceType::Pawn) & !diag_pin;
    // Pawns that can move forward (pseudo legal, rook pin checked later)
    let mut pushable = moveable & empty_bb.shift(-push);
    // Pawns that can move forward two squares
    // Filter:
    // 1. Pawn mush be pushable
    // 2. Pawn mush be double pushable and would block the check if there is one
    // 3. Pawn mush start on push rank
    // 4. Pawn mush not be horizontally pinned
    let double_pushable = pushable
        & (empty_bb & check_mask).shift(-double_push)
        & push_rank
        & (!hv_pin | hv_pin.shift(-double_push));

    // Filter:
    // 1. Push must land on a square within the check_mask (if checked)
    // 2. If pinned vertically, the push must stay on the pin line
    pushable &= check_mask.shift(-push) & (!hv_pin | hv_pin.shift(-push));

    // Handle promotions and normal pawn moves differently
    if (pushable & promo_rank).is_occupied() {
        add_promo_moves::<false>(pushable & promo_rank, move_list, push);
        add_pawn_moves(pushable & !promo_rank, move_list, push, MoveFlag::QuietMove);
    } else {
        add_pawn_moves(pushable, move_list, push, MoveFlag::QuietMove);
    }

    // Add double pawn push moves
    add_pawn_moves(
        double_pushable,
        move_list,
        double_push,
        MoveFlag::DoublePawnPush,
    );
}

/// Generates pawn captures (normal captures, promotion captures, en passant).
/// Considers pins and checks.
fn gen_pawn_captures(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();
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

    // Board state masks
    let enemy_bb = board.occupied_bb(them);
    let promo_rank = Bitboard::promo_rank(us); // Rank *before* promotion
    let diag_pin = board.diag_pin(); // Pawns pinned diagonally have restricted captures
    let hv_pin = board.hv_pin(); // Pawns pinned horizontally cannot capture
    let check_mask = board.check_mask(); // Capture must be on a check_mask square (if checked)

    // --- Normal Captures ---
    // Start with pawns not pinned horizontally
    let movable = board.piece_bb(us, PieceType::Pawn) & !hv_pin;
    // Find pawns where the left capture square has an enemy piece & is in check_mask
    let mut pawn_left = movable & (enemy_bb & check_mask).shift(-left);
    // Filter: If pinned diagonally, capture must be along the pin line
    pawn_left &= !diag_pin | diag_pin.shift(-left);

    // Find pawns where the right capture square has an enemy piece & is in check_mask
    let mut pawn_right = movable & (enemy_bb & check_mask).shift(-right);
    // Filter: If pinned diagonally, capture must be along the pin line
    pawn_right &= !diag_pin | diag_pin.shift(-right);

    // --- En Passant ---
    if let Some(ep_sq) = board.ep() {
        // Check for complex discovered check via en passant (if king, captured pawn, and rook are on same rank)
        if !board.ep_pin() {
            // Target square for the capture move (the EP square itself) must be in check_mask
            let ep_target = unsafe { ep_sq.add_unchecked(-us.forward()) };
            // Calculate potential attacking pawn squares relative to the EP square
            let ep_left = movable
                & (ep_target.bb() & check_mask).shift(ep_left)
                & (!diag_pin | diag_pin.shift(-left));
            let ep_right = movable
                & (ep_target.bb() & check_mask).shift(ep_right)
                & (!diag_pin | diag_pin.shift(-right));

            // Check left attacker
            if ep_left.is_occupied() {
                let from = unsafe { ep_sq.add_unchecked(-left) };
                move_list.add_move(Move::new(from, ep_sq, MoveFlag::EPCapture));
            }

            // Check right attacker
            if ep_right.is_occupied() {
                let from = unsafe { ep_sq.add_unchecked(-right) };
                move_list.add_move(Move::new(from, ep_sq, MoveFlag::EPCapture));
            }
        }
    }

    // --- Add Moves ---
    // Handle promotion captures separately
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

/// Generates legal knight moves.
/// Knights cannot be pinned partially, so they only move if not pinned at all.
/// Moves must land on a square within the `check_mask` if the king is in check.
fn gen_knight_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    // Board state masks
    let check_mask = board.check_mask();
    let pin_mask = board.diag_pin() | board.hv_pin(); // Knights are affected by any pin

    // Get knights that are not absolutely pinned
    let knights = board.piece_bb(us, PieceType::Knight) & !pin_mask;

    // For each non-pinned knight
    knights.for_each(|from| {
        // Get potential destinations using precomputed attacks
        // Filter destinations by the check_mask (must block check or capture checker if checked)
        let dest = leaper_attack(PieceType::Knight, from) & check_mask;
        // Add captures and/or quiet moves based on GenType G
        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

fn gen_diag_slider_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    // Board state masks
    let check_mask = board.check_mask();
    let diag_pin = board.diag_pin(); // Pieces pinned diagonally
    let hv_pin = board.hv_pin(); // Pieces pinned horizontally/vertically
    let all_occ = board.all_occupied_bb();

    // Get relevant pieces
    let pieces =
        (board.piece_bb(us, PieceType::Queen) | board.piece_bb(us, PieceType::Bishop)) & !hv_pin;

    // Separate pinned and non-pinned diagonal movers
    let pinned = pieces & diag_pin;
    let non_pinned = pieces & !diag_pin;

    // Generate moves for pinned pieces
    pinned.for_each(|from| {
        // Attacks considering occupancy, filtered by check_mask AND the pin line itself
        let dest = slider_attack(PieceType::Bishop, from, all_occ) & check_mask & diag_pin;
        add_piece_moves::<G>(board, from, dest, move_list);
    });

    // Generate moves for non-pinned pieces
    non_pinned.for_each(|from| {
        // Attacks considering occupancy, filtered only by check_mask
        let dest = slider_attack(PieceType::Bishop, from, all_occ) & check_mask;
        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

fn gen_hv_slider_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    // Board state masks
    let check_mask = board.check_mask();
    let diag_pin = board.diag_pin(); // Pieces pinned diagonally
    let hv_pin = board.hv_pin(); // Pieces pinned horizontally/vertically
    let all_occ = board.all_occupied_bb();

    // Get relevant pieces
    let pieces =
        (board.piece_bb(us, PieceType::Queen) | board.piece_bb(us, PieceType::Rook)) & !diag_pin;

    // Separate pinned and non-pinned hv movers
    let pinned = pieces & hv_pin;
    let non_pinned = pieces & !hv_pin;

    // Generate moves for pinned pieces
    pinned.for_each(|from| {
        // Attacks considering occupancy, filtered by check_mask AND the pin line itself
        let dest = slider_attack(PieceType::Rook, from, all_occ) & check_mask & hv_pin;
        add_piece_moves::<G>(board, from, dest, move_list);
    });

    // Generate moves for non-pinned pieces
    non_pinned.for_each(|from| {
        // Attacks considering occupancy, filtered only by check_mask
        let dest = slider_attack(PieceType::Rook, from, all_occ) & check_mask;
        add_piece_moves::<G>(board, from, dest, move_list);
    })
}

/// Generates legal king moves.
/// The king cannot move into squares attacked by the opponent. Castling is handled separately.
fn gen_king_moves<G: GenTypeTrait>(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    let from = board.ksq(us);
    // Get potential destinations using precomputed king attacks
    // Filter destinations: must NOT be attacked by the opponent
    let dest = leaper_attack(PieceType::King, from) & !board.attacked();
    // Add captures and/or quiet moves based on GenType G
    // Note: King moves don't use check_mask directly; the attacked squares check handles legality.
    add_piece_moves::<G>(board, from, dest, move_list);
}

/// Generates legal castling moves (WK, WQ, BK, BQ).
/// Called only when the king is not in check and `GenType` allows quiet moves.
/// Legality checks (rights, empty squares, no attacks on path) are done in `add_castling_move`.
fn gen_castling_moves(board: &Board, move_list: &mut MoveList) {
    let us = board.stm();

    // Call the helper for each potential castling move.
    // The helper checks rights and legality internally.
    // Call the helper for each potential castling move.
    // The helper checks rights and legality internally.
    match us {
        Colour::White => {
            add_castling_move(Castling::WK, board, move_list);
            add_castling_move(Castling::WQ, board, move_list); // WQ index
        }
        Colour::Black => {
            add_castling_move(Castling::BK, board, move_list); // BK index
            add_castling_move(Castling::BQ, board, move_list); // BQ index
        }
    }
}
