//! # Module: `zobrist`
//!
//! This module provides functionality for Zobrist hashing, a technique used to efficiently
//! represent and compare board positions in chess. It defines the `Key` type, the `ZobristTable`,
//! and functions for calculating and updating Zobrist keys for various board states.
//!
//! ## Overview
//!
//! Zobrist hashing is a method for generating a unique hash value (a `Key`) for a given board
//! position. This hash value can be used to quickly determine if two board positions are the same,
//! which is crucial for tasks like detecting repeated positions (for the three-fold repetition rule)
//! and for transposition tables in search algorithms.
//!
//! The core idea behind Zobrist hashing is to assign a unique random number to each possible
//! combination of piece, square, side to move, castling rights, and en passant square. The hash
//! value for a board position is then calculated by XORing together the random numbers
//! corresponding to the current state of the board.
//!
//! ## Key Components
//!
//! - **`Key`**: A struct representing a Zobrist key, which is essentially a `u64` value.
//! - **`ZobristTable`**: A struct containing precomputed random numbers for each piece, square,
//!   side to move, castling right, and en passant file.
//! - **`ZOBRIST`**: A global, lazily initialized `ZobristTable` instance.
//!
//! ## Functionality
//!
//! - **Initialization**: The `init_zobrist_table` function initializes the `ZobristTable` with
//!   pseudo-random numbers.
//! - **Key Calculation**: The `Board::calc_key` method calculates the Zobrist key for the current
//!   board position.
//! - **Key Updates**: The `Board` struct provides methods for efficiently updating the Zobrist key
//!   when changes occur on the board, such as:
//!   - `update_castle_key`: Updates the key when castling rights change.
//!   - `update_ep_key`: Updates the key when the en passant square changes.
//!   - `update_side_key`: Updates the key when the side to move changes.
//!   - `update_piece_key`:

use macros::BitOps;
use std::sync::LazyLock;

use crate::core::*;
use crate::utils::PRNG;

/******************************************\
|==========================================|
|              Key Definition              |
|==========================================|
\******************************************/

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, BitOps)]
pub struct Key(u64);

impl Key {
    /// Returns the inner u64 value.
    pub const fn data(self) -> u64 {
        self.0
    }
}

/******************************************\
|==========================================|
|              Zobrist Table               |
|==========================================|
\******************************************/

/// Holds the precomputed pseudo-random numbers for Zobrist hashing.
#[derive(Debug)]
pub struct ZobristTable {
    /// `pieces[PieceType][Colour][Square]`
    pub pieces: [[Key; Square::NUM]; Piece::NUM],
    /// Key for black's turn to move (XORed in if black moves).
    pub side_to_move: Key,
    /// `castling[Castling_Flag]` - Key for each possible castling right combination
    pub castling: [Key; Castling::NUM],
    /// `enpassant[File]` - only the file matters.
    pub enpassant: [Key; File::NUM],
}

/// Global static instance of the Zobrist table, initialized lazily and thread-safely.
pub static ZOBRIST: LazyLock<ZobristTable> = LazyLock::new(init_zobrist_table);

/******************************************\
|==========================================|
|              Initialisation              |
|==========================================|
\******************************************/

/// Initializes the Zobrist table with pseudo-random numbers.
fn init_zobrist_table() -> ZobristTable {
    // Use a fixed seed for reproducibility during development/testing if desired,
    // or a random seed for production.
    let mut rng = PRNG::new(0xDEADBEEFCAFEBABE); // Example fixed seed
    let mut pieces = [[Key::default(); Square::NUM]; Piece::NUM];
    let mut castling = [Key::default(); Castling::NUM];
    let mut enpassant = [Key::default(); File::NUM];

    for pc in Piece::iter() {
        for sq in Square::iter() {
            pieces[pc as usize][sq as usize] = Key(rng.random_u64());
        }
    }

    let side_to_move = Key(rng.random_u64());

    // One key for each individual castling right flag
    for i in 0..16 {
        castling[i] = Key(rng.random_u64());
    }

    for file in File::iter() {
        enpassant[file as usize] = Key(rng.random_u64());
    }

    ZobristTable {
        pieces,
        side_to_move,
        castling,
        enpassant,
    }
}

/******************************************\
|==========================================|
|              Access Functions            |
|==========================================|
\******************************************/

/// Gets the Zobrist key for a specific piece on a square.
#[inline]
pub fn piece_key(piece: Piece, sq: Square) -> Key {
    ZOBRIST.pieces[piece as usize][sq as usize]
}

/// Gets the Zobrist key for flipping the side to move.
#[inline]
pub fn side_key() -> Key {
    ZOBRIST.side_to_move
}

/// Gets the Zobrist key for a specific castling right flag.
/// Note: Assumes Castling flags are powers of 2 or can be mapped to 0..3.
#[inline]
pub fn castle_key(flag: Castling) -> Key {
    // This mapping depends on how Castling flags are defined.
    // If WK=1, WQ=2, BK=4, BQ=8 (powers of 2):
    ZOBRIST.castling[flag.0 as usize]
}

/// Gets the Zobrist key for an en passant file.
#[inline]
pub fn ep_key(file: File) -> Key {
    ZOBRIST.enpassant[file as usize]
}

/******************************************\
|==========================================|
|            Board Implementation          |
|==========================================|
\******************************************/

impl Board {
    pub fn calc_key(&self) -> Key {
        let mut key = Key::default();

        // 1. Pieces
        // Iterate through pieces using the board array for potentially better cache locality
        for sq in Square::iter() {
            if let Some(piece) = self.on(sq) {
                key ^= piece_key(piece, sq);
            }
        }

        // 2. Side to Move
        if self.side_to_move == Colour::Black {
            key ^= side_key();
        }

        // 3. Castling Rights
        // XOR based on the entire castling rights state directly
        // Assumes ZOBRIST.castling is indexed by the Castling bitmask value (0-15)
        key ^= castle_key(self.state.castle);

        // 4. En Passant File
        if let Some(ep_square) = self.state.enpassant {
            // Only the file of the target square matters for the key
            key ^= ep_key(ep_square.file());
        }

        key
    }

    /// Updates the Zobrist key based on a change in castling rights.
    pub fn update_castle_key(&mut self, old_castle: Castling, new_castle: Castling) {
        // XOR out the old castling rights
        self.state.key ^= castle_key(old_castle);
        // XOR in the new castling rights
        self.state.key ^= castle_key(new_castle);
    }

    /// Updates the Zobrist key based on a change in the en passant square.
    pub fn update_ep_key(&mut self, old_ep: Option<Square>, new_ep: Option<Square>) {
        // XOR out the old en passant square (if any)
        if let Some(old_ep_square) = old_ep {
            self.state.key ^= ep_key(old_ep_square.file());
        }
        // XOR in the new en passant square (if any)
        if let Some(new_ep_square) = new_ep {
            self.state.key ^= ep_key(new_ep_square.file());
        }
    }

    /// Updates the Zobrist key based on a change in the side to move.
    pub fn update_side_key(&mut self) {
        self.state.key ^= side_key();
    }

    /// Updates the Zobrist key based on a piece being added or removed.
    pub fn update_piece_key(&mut self, piece: Piece, sq: Square) {
        self.state.key ^= piece_key(piece, sq);
    }
}

#[cfg(test)]
mod tests {
    use super::fen::*;
    use super::*;

    #[test]
    fn key_xor() {
        let k1 = Key(0x12345);
        let k2 = Key(0xABCDE);
        let k3 = k1 ^ k2;
        assert_eq!(k3.0, 0x12345 ^ 0xABCDE);
        let mut k4 = k1;
        k4 ^= k2;
        assert_eq!(k4, k3);
        k4 ^= k2; // XORing again restores original
        assert_eq!(k4, k1);
    }

    #[test]
    fn zobrist_table_init() {
        // Force initialization
        LazyLock::force(&ZOBRIST);
        // Basic checks: ensure keys are not zero (highly unlikely with random u64)
        assert_ne!(piece_key(Piece::WhitePawn, Square::E4).0, 0);
        assert_ne!(piece_key(Piece::BlackKing, Square::G8).0, 0);
        assert_ne!(side_key().0, 0);
        assert_ne!(castle_key(Castling::WK).0, 0);
        assert_ne!(ep_key(File::FileD).0, 0);

        // Check uniqueness (highly likely, but not guaranteed)
        assert_ne!(
            piece_key(Piece::WhitePawn, Square::E4),
            piece_key(Piece::WhitePawn, Square::E5)
        );
        assert_ne!(
            piece_key(Piece::WhitePawn, Square::E4),
            piece_key(Piece::BlackPawn, Square::E4)
        );
        assert_ne!(castle_key(Castling::WK), castle_key(Castling::WQ));
    }

    // Helper to set up board from FEN and calculate key
    fn get_key_from_fen(fen: &str) -> Key {
        let mut board = Board::new();
        // Use expect here as tests rely on valid FENs
        board
            .set(fen)
            .expect(&format!("Test FEN failed to parse: {}", fen));
        // After set(), the key should be calculated and stored in board.state.key
        // We also recalculate it here to test calculate_zobrist_key directly
        let calculated_key = board.calc_key();
        assert_eq!(
            board.state.key, calculated_key,
            "Stored key differs from recalculated key for FEN: {}",
            fen
        );
        calculated_key
    }

    #[test]
    fn test_startpos_key() {
        let key_start = get_key_from_fen(START_FEN);
        assert_ne!(key_start.data(), 0, "Startpos key should not be zero");

        // Verify it's consistent
        let key_start_again = get_key_from_fen(START_FEN);
        assert_eq!(key_start, key_start_again, "Startpos key is not consistent");
    }

    #[test]
    fn test_empty_board_key() {
        // Empty board, white to move, KQkq rights (as per EMPTY_FEN spec)
        let key_empty_w_kqkq = get_key_from_fen(EMPTY_FEN);

        // Empty board, black to move, KQkq rights
        let key_empty_b_kqkq = get_key_from_fen("8/8/8/8/8/8/8/8 b KQkq - 0 1");
        assert_ne!(
            key_empty_w_kqkq, key_empty_b_kqkq,
            "Keys should differ based on side to move"
        );
        // Check that XORing side key works
        assert_eq!(key_empty_b_kqkq, key_empty_w_kqkq ^ side_key());

        // Empty board, white to move, no rights
        let key_empty_w_none = get_key_from_fen("8/8/8/8/8/8/8/8 w - - 0 1");
        assert_ne!(
            key_empty_w_kqkq, key_empty_w_none,
            "Keys should differ based on castling rights"
        );
        // Check that XORing castling keys works
        assert_eq!(
            key_empty_w_none,
            key_empty_w_kqkq ^ castle_key(Castling::NONE) ^ castle_key(Castling::ALL)
        ); // XORing ALL rights out
    }

    #[test]
    fn test_piece_difference() {
        let key_start = get_key_from_fen(START_FEN);
        // Same as startpos but e.g. knight on c3 instead of b1
        let key_nc3 =
            get_key_from_fen("rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 0 1");
        assert_ne!(
            key_start, key_nc3,
            "Keys should differ based on piece placement"
        );

        // Check that XORing pieces works (Nb1 out, Nc3 in)
        let expected_key = key_start
            ^ piece_key(Piece::WhiteKnight, Square::B1)
            ^ piece_key(Piece::WhiteKnight, Square::C3);
        assert_eq!(
            key_nc3, expected_key,
            "Manual piece XOR did not match calculated key"
        );
    }

    #[test]
    fn test_castling_difference() {
        let key_kqkq = get_key_from_fen(START_FEN);
        let key_kqk = get_key_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQk - 0 1");
        let key_none = get_key_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1");

        assert_ne!(key_kqkq, key_kqk, "Keys should differ for BQ right removal");
        assert_ne!(
            key_kqk, key_none,
            "Keys should differ when all rights removed"
        );

        // Check XORing castling keys
        let expected_key_kqk = key_kqkq ^ castle_key(Castling::ALL) ^ castle_key(Castling(7)); // XOR out BQ
        assert_eq!(
            key_kqk, expected_key_kqk,
            "Manual castling XOR (BQ) did not match"
        );

        // Test XORing entire state
        let expected_key_none = key_kqkq ^ castle_key(Castling::ALL) ^ castle_key(Castling::NONE); // XOR out ALL
        assert_eq!(
            key_none, expected_key_none,
            "Manual castling XOR (ALL) did not match"
        );

        // Test XORing back
        let expected_key_kqkq_again =
            key_none ^ castle_key(Castling::NONE) ^ castle_key(Castling::ALL); // XOR ALL back in
        assert_eq!(
            key_kqkq, expected_key_kqkq_again,
            "XORing castling rights back did not restore key"
        );
    }

    #[test]
    fn test_enpassant_difference() {
        // Position after 1. e4
        let key_no_ep =
            get_key_from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1");
        // Position after 1. e4 c5 2. Nf3 d5 3. exd5 Qxd5 4. Nc3 Qd8 5. d4 cxd4 6. Qxd4 Qxd4 7. Nxd4 a6 8. Nd5 Ra7 9. Be3 e5 10. Nf5 (Black to move, EP possible on e6)
        let key_ep_e6 =
            get_key_from_fen("rnb1kbnr/p4p1p/4p3/1p1N4/3N4/4B3/PPP2PPP/R3KB1R b KQkq - 1 10"); // Example with EP on e-file
        // Same position but different EP file (hypothetical)
        let key_ep_f6 =
            get_key_from_fen("rnb1kbnr/p4p1p/5p2/1p1N4/3N4/4B3/PPP2PPP/R3KB1R b KQkq f6 1 10"); // Hypothetical EP on f-file

        assert_ne!(key_no_ep, key_ep_e6, "Keys should differ with EP square");
        assert_ne!(key_ep_e6, key_ep_f6, "Keys should differ with EP file");

        // Check XORing EP keys
        let expected_key_ep_e6 = key_no_ep ^ ep_key(File::FileE); // XOR in EP on e-file
        // Note: This assumes key_no_ep had black to move and correct castling/pieces for the target pos.
        // A better test compares two positions differing *only* by EP.
        let base_fen_str = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1";
        let ep_fen_str = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"; // Add EP e3

        let key_base = get_key_from_fen(base_fen_str);
        let key_ep = get_key_from_fen(ep_fen_str);

        assert_ne!(key_base, key_ep, "Keys should differ only by EP square");
        assert_eq!(
            key_ep,
            key_base ^ ep_key(File::FileE),
            "Manual EP XOR did not match"
        );
        assert_eq!(
            key_base,
            key_ep ^ ep_key(File::FileE),
            "XORing EP back did not restore key"
        );
    }

    #[test]
    fn test_kiwipete_key() {
        // Test against a known complex position
        let key_kiwi = get_key_from_fen(TRICKY_FEN);
        assert_ne!(key_kiwi.data(), 0, "Kiwipete key should not be zero");

        // Compare against startpos
        let key_start = get_key_from_fen(START_FEN);
        assert_ne!(
            key_kiwi, key_start,
            "Kiwipete key should differ from startpos"
        );
    }
}
