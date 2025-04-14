//! # Module: `zobrist`
//!
//! Provides Zobrist hashing functionality for chess board states.
//!
//! ## Overview
//!
//! Zobrist hashing assigns a unique pseudo-random number (`Key`, a `u64`) to each possible
//! element of a chess position:
//! - Each piece type on each square (`Piece` + `Square`)
//! - The side to move (`Colour::Black`)
//! - Each individual castling right (`Castling` flag)
//! - Each possible en passant target file (`File`)
//!
//! The Zobrist key for a complete board position is calculated by XORing together the keys
//! corresponding to the elements currently present in that position.
//!
//! ### Properties of XOR
//!
//! XOR (`^`) is used because it's:
//! - **Fast:** A simple bitwise operation.
//! - **Reversible:** `(A ^ B) ^ B = A`. This allows efficient incremental updates. If a piece
//!   moves from square A to B, the board's key can be updated by `key ^= piece_key(piece, A) ^ piece_key(piece, B)`.
//! - **Distributive:** Order doesn't matter: `A ^ B ^ C = C ^ A ^ B`.
//!
//! ### Use Cases
//!
//! - **Transposition Tables:** Storing previously evaluated positions and their scores/best moves
//!   in a hash table, keyed by the Zobrist key, to avoid re-calculating the same lines during search.
//! - **Repetition Detection:** Detecting three-fold (or fifty-move rule) repetitions by checking if
//!   the current position's Zobrist key has appeared before in the game history.
//!
//! ## Key Components
//!
//! - **`Key`**: A wrapper struct around `u64` representing a Zobrist key. Derives `BitOps` for easy XORing.
//! - **`ZobristTable`**: Stores the precomputed pseudo-random `Key` values for all possible board elements.
//! - **`ZOBRIST`**: A global `LazyLock<ZobristTable>` providing thread-safe, on-demand initialization
//!   and access to the precomputed keys. Using `LazyLock` avoids static initialization overhead until
//!   the table is first accessed.
//! - **`init_zobrist_table`**: The function used by `LazyLock` to populate the `ZobristTable` using a `PRNG`.
//! - **Access Functions**: (`piece_key`, `side_key`, `castle_key`, `ep_key`) provide convenient access to keys in the global table.
//! - **`Board` Methods**:
//!     - `calc_key`: Calculates the full key from scratch. Useful for initialization or verification.
//!     - `update_*_key`: Methods for incrementally updating the board's stored key when the state changes (e.g., piece moves, side changes, castling rights change, EP square changes). These are crucial for performance during move generation and search.

use macros::BitOps;
use std::sync::LazyLock;

use crate::core::*;
use crate::utils::PRNG;

/******************************************\
|==========================================|
|              Key Definition              |
|==========================================|
\******************************************/

/// Represents a Zobrist hash key, wrapping a `u64`.
///
/// Provides type safety over using raw `u64` for Zobrist keys.
/// Derives `BitOps` (`^`, `^=`) for convenient XOR operations, which are
/// fundamental to calculating and updating Zobrist keys.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, BitOps)]
pub struct Key(
    /// The underlying 64-bit unsigned integer representing the hash value.
    pub u64,
);

impl Key {
    /// Returns the inner `u64` value of the Zobrist key.
    #[inline]
    pub const fn data(self) -> u64 {
        self.0
    }
}

/******************************************\
|==========================================|
|              Zobrist Table               |
|==========================================|
\******************************************/

/// Stores precomputed pseudo-random numbers (Keys) for Zobrist hashing.
///
/// Each field holds keys corresponding to a specific aspect of the board state.
/// These keys are generated once at startup (lazily) and then used for all
/// Zobrist calculations.
#[derive(Debug)]
pub struct ZobristTable {
    /// Stores a unique key for each piece type on each square.
    /// Indexed as `pieces[piece_index][square_index]`.
    /// `piece_index` typically corresponds to `Piece as usize`.
    /// `square_index` typically corresponds to `Square as usize`.
    pub pieces: [[Key; Square::NUM]; Piece::NUM], // Piece::NUM = 12, Square::NUM = 64

    /// A single key that is XORed into the board's hash if it's Black's turn to move.
    /// If it's White's turn, this key is not included (or XORed out).
    pub side_to_move: Key,

    /// Stores keys corresponding to combinations of castling rights.
    /// Indexed directly by the `Castling` bitmask value (0-15).
    /// `castling[castling_rights.bits() as usize]`.
    /// For example, `castling[Castling::WK.bits() as usize]` holds the key for White kingside rights.
    /// `castling[Castling::ALL.bits() as usize]` holds the key for all four rights (WK | WQ | BK | BQ).
    pub castling: [Key; Castling::NUM], // Castling::NUM = 16

    /// Stores keys corresponding to the *file* of a potential en passant target square.
    /// Indexed by `file as usize`. Only the file is needed because an en passant capture
    /// is only possible immediately after a two-square pawn advance, uniquely determining the rank.
    pub enpassant: [Key; File::NUM], // File::NUM = 8
}

/// Global static instance of the Zobrist table.
///
/// Initialized lazily using `LazyLock`, meaning the `init_zobrist_table` function
/// is only called the first time `ZOBRIST` is accessed. This initialization is
/// thread-safe. Using `LazyLock` avoids potential overhead of initializing the
/// table at program startup if it's not immediately needed.
pub static ZOBRIST: LazyLock<ZobristTable> = LazyLock::new(init_zobrist_table);

/******************************************\
|==========================================|
|              Initialisation              |
|==========================================|
\******************************************/

/// Initializes the global `ZobristTable` with pseudo-random `u64` keys.
///
/// This function is called internally by `LazyLock` when `ZOBRIST` is first accessed.
/// It uses a `PRNG` (Pseudo-Random Number Generator) to fill the table arrays.
///
/// Using a fixed seed for the `PRNG` (like `0xDEADBEEFCAFEBABE`) ensures that
/// the generated Zobrist keys are deterministic across program runs. This is crucial
/// for reproducible testing and debugging. For a production engine where unpredictability
/// might be desired (though usually not necessary for Zobrist), a seed derived from
/// system time or another random source could be used.
///
/// # Returns
///
/// A fully populated `ZobristTable`.
fn init_zobrist_table() -> ZobristTable {
    // Use a fixed seed for deterministic keys, essential for testing and reproducibility.
    let mut rng = PRNG::new(0xDEADBEEFCAFEBABE); // Example fixed seed

    // Initialize arrays with default (zero) keys.
    let mut pieces = [[Key::default(); Square::NUM]; Piece::NUM];
    let mut castling = [Key::default(); Castling::NUM];
    let mut enpassant = [Key::default(); File::NUM];

    // Generate a unique key for each piece on each square.
    for pc in Piece::iter() {
        for sq in Square::iter() {
            pieces[pc as usize][sq as usize] = Key(rng.random_u64());
        }
    }

    // Generate the key for indicating Black to move.
    let side_to_move = Key(rng.random_u64());

    // Generate a unique key for each possible combination of castling rights (0-15).
    // It's important that ZOBRIST.castling[rights.bits()] gives the key for that specific combination.
    for i in 0..Castling::NUM {
        // Iterate 0 through 15
        castling[i] = Key(rng.random_u64());
    }

    // Generate a unique key for each possible en passant file.
    for file in File::iter() {
        enpassant[file as usize] = Key(rng.random_u64());
    }

    // Return the fully initialized table.
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

/// Gets the precomputed Zobrist key for a specific piece on a specific square.
///
/// # Arguments
/// * `piece`: The `Piece` (including color).
/// * `sq`: The `Square`.
///
/// # Returns
/// The corresponding `Key` from the global `ZOBRIST` table.
///
/// `#[inline]` is suggested as this is a very frequent, simple lookup.
#[inline]
pub fn piece_key(piece: Piece, sq: Square) -> Key {
    // Access the precomputed key using piece and square as indices.
    // Assumes Piece and Square can be safely cast to usize for indexing.
    ZOBRIST.pieces[piece as usize][sq as usize]
}

/// Gets the precomputed Zobrist key used to indicate the side to move.
///
/// This key is XORed into the board hash only when it is Black's turn.
///
/// # Returns
/// The `side_to_move` `Key` from the global `ZOBRIST` table.
///
/// `#[inline]` is suggested as this is a frequent, simple lookup.
#[inline]
pub fn side_key() -> Key {
    ZOBRIST.side_to_move
}

/// Gets the precomputed Zobrist key for a specific set of castling rights.
///
/// # Arguments
/// * `flag`: The `Castling` bitmask representing the rights (e.g., `Castling::WK | Castling::BQ`).
///
/// # Returns
/// The corresponding `Key` from the `ZOBRIST.castling` array, indexed by the `flag`'s bit value.
///
/// `#[inline]` is suggested as this is a frequent, simple lookup.
#[inline]
pub fn castle_key(flag: Castling) -> Key {
    // Access the precomputed key using the castling flag's bits as the index (0-15).
    ZOBRIST.castling[flag.0 as usize]
}

/// Gets the precomputed Zobrist key for a potential en passant capture file.
///
/// # Arguments
/// * `file`: The `File` of the square *behind* the pawn that just moved two squares.
///           This is the file of the potential en passant target square.
///
/// # Returns
/// The corresponding `Key` from the `ZOBRIST.enpassant` array, indexed by the `file`.
///
/// `#[inline]` is suggested as this is a frequent, simple lookup.
#[inline]
pub fn ep_key(file: File) -> Key {
    // Access the precomputed key using the file as the index.
    // Assumes File can be safely cast to usize for indexing.
    ZOBRIST.enpassant[file as usize]
}

/******************************************\
|==========================================|
|            Board Implementation          |
|==========================================|
\******************************************/

impl Board {
    /// Calculates the Zobrist key for the current board state from scratch.
    ///
    /// This method iterates through all pieces, checks the side to move,
    /// castling rights, and en passant square, XORing the corresponding keys
    /// from the global `ZOBRIST` table to produce the final key.
    ///
    /// This is typically used when initially setting up a board from a FEN string
    /// or for verifying the correctness of incrementally updated keys during debugging.
    /// For performance during move generation and search, incremental updates
    /// (`update_piece_key`, `update_castle_key`, etc.) are preferred.
    ///
    /// # Returns
    ///
    /// The calculated `Key` for the current board position.
    pub fn calc_key(&self) -> Key {
        let mut key = Key::default(); // Start with a zero key

        // 1. XOR keys for all pieces on the board
        // Iterate through squares and XOR in the key if a piece is present.
        for sq in Square::iter() {
            if let Some(piece) = self.on(sq) {
                key ^= piece_key(piece, sq);
            }
        }

        // 2. XOR side key if it's Black's turn
        if self.side_to_move == Colour::Black {
            key ^= side_key();
        }

        // 3. XOR the key corresponding to the current castling rights state.
        // Assumes ZOBRIST.castling is indexed by the Castling bitmask value (0-15).
        key ^= castle_key(self.state.castle);

        // 4. XOR the en passant key if an EP square exists.
        if let Some(ep_square) = self.state.enpassant {
            // Only the file of the target square matters for the key.
            key ^= ep_key(ep_square.file());
        }

        key // Return the final calculated key
    }

    /// Incrementally updates the board's Zobrist key when castling rights change.
    ///
    /// This is much faster than recalculating the entire key using `calc_key`.
    /// It works by XORing out the key for the old castling state and XORing in
    /// the key for the new state.
    ///
    /// # Arguments
    ///
    /// * `old_castle`: The `Castling` rights *before* the change.
    /// * `new_castle`: The `Castling` rights *after* the change.
    ///
    /// This method should be called whenever an operation (like moving a king or rook,
    /// or a rook being captured) modifies the `board.state.castle` value.
    #[inline]
    pub fn update_castle_key(&mut self, old_castle: Castling, new_castle: Castling) {
        // XOR out the key corresponding to the old castling rights
        self.state.key ^= castle_key(old_castle);
        // XOR in the key corresponding to the new castling rights
        self.state.key ^= castle_key(new_castle);
    }

    /// Incrementally updates the board's Zobrist key when the en passant square changes.
    ///
    /// XORs out the key for the old en passant file (if one existed) and XORs in the
    /// key for the new en passant file (if one exists).
    ///
    /// # Arguments
    ///
    /// * `old_ep`: The `Option<Square>` representing the en passant square *before* the change.
    /// * `new_ep`: The `Option<Square>` representing the en passant square *after* the change.
    ///
    /// This should be called after any move that potentially creates or clears an
    /// en passant target square.
    #[inline]
    pub fn update_ep_key(&mut self, old_ep: Option<Square>, new_ep: Option<Square>) {
        // XOR out the old en passant file's key, if an old EP square existed.
        if let Some(old_ep_square) = old_ep {
            self.state.key ^= ep_key(old_ep_square.file());
        }
        // XOR in the new en passant file's key, if a new EP square exists.
        if let Some(new_ep_square) = new_ep {
            self.state.key ^= ep_key(new_ep_square.file());
        }
    }

    /// Incrementally updates the board's Zobrist key when the side to move changes.
    ///
    /// Simply XORs the `side_key()` into the board's key. Since `key ^ side ^ side = key`,
    /// calling this function toggles the side-to-move component of the hash.
    ///
    /// This should be called exactly once per move made (typically at the end of `make_move`).
    #[inline]
    pub fn update_side_key(&mut self) {
        self.state.key ^= side_key();
    }

    /// Incrementally updates the board's Zobrist key when a piece is added or removed.
    ///
    /// XORs the key corresponding to the given `piece` at the given `sq`.
    /// Because XORing twice cancels out (`key ^ piece ^ piece = key`), this single function
    /// handles both placing a piece (XORing its key in) and removing a piece (XORing its key out).
    ///
    /// # Arguments
    ///
    /// * `piece`: The `Piece` being added or removed.
    /// * `sq`: The `Square` where the piece is being added or removed.
    ///
    /// This should be called whenever a piece's position changes during `make_move` or `unmake_move`.
    /// For a move from `from_sq` to `to_sq`, you would typically call:
    /// `update_piece_key(piece, from_sq)` (remove from old square)
    /// `update_piece_key(piece, to_sq)` (add to new square)
    #[inline]
    pub fn update_piece_key(&mut self, piece: Piece, sq: Square) {
        self.state.key ^= piece_key(piece, sq);
    }
}

#[cfg(test)]
mod tests {
    // Note: Test documentation is usually less critical than library code documentation.
    // Existing tests seem clear in their intent.
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
