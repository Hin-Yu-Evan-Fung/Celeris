//! # Module: `zobrist`
//!
//! Provides Zobrist hashing functionality for chess board states.
//!
//! ## Overview
//!
//! Zobrist hashing assigns a unique pseudo-random number (`Key`, a `Key`) to each possible
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
//! - **`Key`**: A wrapper struct around `Key` representing a Zobrist key. Derives `BitOps` for easy XORing.
//!   - Includes `toggle_*` methods for efficient incremental updates.
//! - **`ZobristTable`**: Stores the precomputed pseudo-random `Key` values for all possible board elements.
//! - **`ZOBRIST`**: A global `const ZobristTable` providing thread-safe, compile-time initialization
//!   and access to the precomputed keys.
//! - **`init_zobrist_table`**: The `const fn` used to populate the `ZobristTable`.
//! - **Access Functions**: (`piece_key`, `side_key`, `castle_key`, `ep_key`) provide convenient access to keys in the global table.
//! - **`Board` Methods**:
//!     - `calc_key`: Calculates the full key from scratch. Useful for initialization or verification.
//!     // Note: Board update methods are not shown here but would typically use the Key::toggle_* methods.

use super::Board;
use crate::core::*;
use crate::utils::PRNG;

/******************************************\
|==========================================|
|              Key Definition              |
|==========================================|
\******************************************/

pub type Key = u64;

/******************************************\
|==========================================|
|                Key Bundle                |
|==========================================|
\******************************************/

#[derive(Debug, Copy, Clone, Default)]
pub struct KeyBundle {
    /// The underlying 64-bit unsigned integer representing the hash value.
    pub key: Key,
    pub pawn_key: Key,
    pub non_pawn_key: [Key; Colour::NUM],
}

impl KeyBundle {
    #[inline]
    pub fn toggle_piece(&mut self, piece: Piece, sq: Square) {
        if piece.pt() as u8 == PieceType::Pawn as u8 {
            self.pawn_key ^= piece_key(piece, sq);
        } else {
            self.non_pawn_key[piece.colour() as usize] ^= piece_key(piece, sq);
        }

        self.key ^= piece_key(piece, sq);
    }

    #[inline]
    pub fn toggle_castle(&mut self, flag: Castling) {
        self.key ^= castle_key(flag); // Access inner Key for XOR
    }

    /// Toggles (XORs) the key corresponding to the side to move into this `Key`.
    ///
    /// This is used for incrementally updating a board's Zobrist key when the
    /// side to move changes (typically after a move is made). XORing with the
    /// `side_key` effectively flips the part of the hash that indicates whether
    /// it's Black's turn.
    #[inline]
    pub fn toggle_colour(&mut self) {
        // XOR self with the precomputed key indicating Black to move.
        // Uses the global ZOBRIST table via the side_key helper.
        self.key ^= side_key(); // Access inner Key for XOR
    }

    /// Toggles (XORs) the key corresponding to the enpassant square into this `Key`.
    ///
    /// This is used for incrementally updating a board's Zobrist key when the
    /// enpassant square changes (typically after a move is made). XORing with the
    /// `enpassant_key(file)` effectively flips the part of the hash that indicates whether
    /// it's Black's turn.
    #[inline]
    pub fn toggle_ep(&mut self, file: File) {
        // XOR self with the precomputed key indicating Black to move.
        // Uses the global ZOBRIST table via the side_key helper.
        self.key ^= ep_key(file); // Access inner Key for XOR
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
/// These keys are generated once at compile time and then used for all
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
    /// `castling[castling_rights as usize]`.
    /// For example, `castling[Castling::WK as usize]` holds the key for White kingside rights.
    /// `castling[Castling::ALL as usize]` holds the key for all four rights (WK | WQ | BK | BQ).
    pub castling: [Key; Castling::NUM], // Castling::NUM = 16

    /// Stores keys corresponding to the *file* of a potential en passant target square.
    /// Indexed by `file as usize`. Only the file is needed because an en passant capture
    /// is only possible immediately after a two-square pawn advance, uniquely determining the rank.
    pub enpassant: [Key; File::NUM], // File::NUM = 8
}

/// Global static instance of the Zobrist table.
///
/// Initialized at compile time using `const fn`. This ensures the table is
/// available immediately without runtime initialization overhead or lazy locking.
pub const ZOBRIST: ZobristTable = init_zobrist_table();

/******************************************\
|==========================================|
|              Initialisation              |
|==========================================|
\******************************************/

/// Initializes the global `ZobristTable` with pseudo-random `Key` keys using `const fn`.
///
/// This function is evaluated at compile time to populate `ZOBRIST`.
/// It uses a `PRNG` (Pseudo-Random Number Generator) to fill the table arrays.
///
/// Using a fixed seed for the `PRNG` (like `0xDEADBEEFCAFEBABE`) ensures that
/// the generated Zobrist keys are deterministic across program runs and compilations.
/// This is crucial for reproducible testing, debugging, and consistent transposition table behavior.
///
/// # Returns
///
/// A fully populated `ZobristTable`.
const fn init_zobrist_table() -> ZobristTable {
    // Use a fixed seed for deterministic keys, essential for testing and reproducibility.
    let mut rng = PRNG::new(0xDEADBEEFCAFEBABE); // Example fixed seed

    // Initialize arrays with default (zero) keys.
    let mut pieces = [[0; Square::NUM]; Piece::NUM];
    let mut castling = [0; Castling::NUM];
    let mut enpassant = [0; File::NUM];

    // Generate a unique key for each piece on each square.
    let mut i = 0;
    while i < Piece::NUM {
        let mut j = 0;
        while j < Square::NUM {
            pieces[i][j] = rng.random_u64();
            j += 1;
        }
        i += 1;
    }

    // Generate the key for indicating Black to move.
    let side_to_move = rng.random_u64();

    // Generate a unique key for each possible combination of castling rights (0-15).
    // It's important that ZOBRIST.castling[rights.bits()] gives the key for that specific combination.
    i = 0;
    while i < Castling::NUM {
        // Iterate 0 through 15
        castling[i] = rng.random_u64();
        i += 1;
    }

    // Generate a unique key for each possible en passant file.
    i = 0;
    while i < File::NUM {
        enpassant[i] = rng.random_u64();
        i += 1; // Increment loop counter
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
    unsafe {
        *ZOBRIST
            .pieces
            .get_unchecked(piece as usize)
            .get_unchecked(sq as usize)
    }
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
    unsafe { *ZOBRIST.castling.get_unchecked(flag.0 as usize) }
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
    unsafe { *ZOBRIST.enpassant.get_unchecked(file as usize) }
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
    /// using `Key::toggle_*` methods are preferred when making/unmaking moves.
    ///
    /// # Returns
    ///
    /// The calculated `Key` for the current board position.
    pub fn calc_key(&self) -> Key {
        let mut key = 0; // Start with a zero key

        // 1. XOR keys for all pieces on the board
        // Iterate through squares and XOR in the key if a piece is present.
        let mut i = 0;
        while i < Square::NUM {
            let sq = unsafe { Square::from_unchecked(i as u8) };
            if let Some(piece) = self.on(sq) {
                // Use the toggle_piece method for clarity, even though direct XOR is equivalent here
                key ^= piece_key(piece, sq);
            }
            i += 1;
        }

        // 2. XOR side key if it's Black's turn
        if self.side_to_move as u8 == Colour::Black as u8 {
            key ^= side_key();
        }

        // 3. XOR the key corresponding to the current castling rights state.
        // Assumes ZOBRIST.castling is indexed by the Castling bitmask value (0-15).
        key ^= castle_key(self.state.castle);

        // 4. XOR the en passant key if an EP square exists.
        if let Some(ep_square) = self.state.enpassant {
            // Only the file of the target square matters for the key.
            // Direct XOR is fine here as ep_key gives the specific key needed.
            key ^= ep_key(ep_square.file());
        }

        key // Return the final calculated key
    }

    /// Calculates the Zobrist key considering only the pawns on the board.
    ///
    /// This is useful for pawn structure evaluation or pawn hash tables.
    ///
    /// # Returns
    ///
    /// The calculated `Key` representing only the pawn configuration.
    pub fn calc_pawn_key(&self) -> Key {
        let mut key = 0; // Start with a zero key

        // Iterate through squares and XOR in the key if a pawn is present.
        let mut i = 0;
        while i < Square::NUM {
            let sq = unsafe { Square::from_unchecked(i as u8) };

            // Check if there is a piece AND if that piece is a pawn
            if let Some(piece) = self.on(sq) {
                if piece.pt() as u8 == PieceType::Pawn as u8 {
                    key ^= piece_key(piece, sq);
                }
            }
            i += 1;
        }

        key // Return the final calculated key
    }

    /// Calculates the Zobrist Key consider only the non pawns on the board for a particular side
    pub fn calc_non_pawn_key(&self) -> [Key; Colour::NUM] {
        let mut keys = [0; Colour::NUM]; // Start with a zero

        let mut i = 0;
        while i < Square::NUM {
            let sq = unsafe { Square::from_unchecked(i as u8) };

            // Check if there is a piece AND if that piece is a pawn
            if let Some(piece) = self.on(sq) {
                if piece.pt() as u8 != PieceType::Pawn as u8 {
                    keys[piece.colour() as usize] ^= piece_key(piece, sq);
                }
            }
            i += 1;
        }

        keys // Return the final calculated key
    }
}

#[cfg(test)]
mod tests {
    // Note: Test documentation is usually less critical than library code documentation.
    // Existing tests seem clear in their intent.
    use super::super::fen::*;
    use super::*;

    #[test]
    fn key_xor() {
        let k1 = 0x12345;
        let k2 = 0xABCDE;
        let k3 = k1 ^ k2;
        assert_eq!(k3, 0x12345 ^ 0xABCDE);
        let mut k4 = k1;
        k4 ^= k2;
        assert_eq!(k4, k3);
        k4 ^= k2; // XORing again restores original
        assert_eq!(k4, k1);
    }

    #[test]
    fn zobrist_table_init() {
        // Accessing ZOBRIST directly ensures it's initialized (as it's const)
        // Basic checks: ensure keys are not zero (highly unlikely with random Key)
        assert_ne!(piece_key(Piece::WhitePawn, Square::E4), 0);
        assert_ne!(piece_key(Piece::BlackKing, Square::G8), 0);
        assert_ne!(side_key(), 0);
        assert_ne!(castle_key(Castling::WK), 0);
        assert_ne!(ep_key(File::FileD), 0);

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
        assert_ne!(castle_key(Castling::ALL), castle_key(Castling::NONE));
        assert_ne!(ep_key(File::FileA), ep_key(File::FileH));
    }

    // --- Tests for Key::toggle_* methods ---

    #[test]
    fn test_toggle_piece() {
        let piece = Piece::WhiteKnight;
        let sq = Square::C3;
        let p_key = piece_key(piece, sq); // Get the key for WN on C3

        // Test starting from zero
        let mut key1 = 0;
        key1 ^= piece_key(piece, sq);
        assert_eq!(key1, p_key, "Toggle piece from zero failed");
        key1 ^= piece_key(piece, sq); // Toggle back
        assert_eq!(key1, 0, "Toggle piece back to zero failed");

        // Test starting from arbitrary value
        let initial_key = 0xABCDEF1234567890;
        let mut key2 = initial_key;
        key2 ^= piece_key(piece, sq);
        assert_eq!(
            key2,
            initial_key ^ p_key,
            "Toggle piece from non-zero failed"
        );
        key2 ^= piece_key(piece, sq); // Toggle back
        assert_eq!(key2, initial_key, "Toggle piece back to non-zero failed");
    }

    #[test]
    fn test_toggle_castle() {
        let flag = Castling::BQ; // Black Queenside
        let c_key = castle_key(flag);

        // Test starting from zero
        let mut key1 = 0;
        key1 ^= castle_key(flag);
        assert_eq!(key1, c_key, "Toggle castle from zero failed");
        key1 ^= castle_key(flag); // Toggle back
        assert_eq!(key1, 0, "Toggle castle back to zero failed");

        // Test starting from arbitrary value
        let initial_key = 0x1122334455667788;
        let mut key2 = initial_key;
        key2 ^= castle_key(flag);
        assert_eq!(
            key2,
            initial_key ^ c_key,
            "Toggle castle from non-zero failed"
        );
        key2 ^= castle_key(flag); // Toggle back
        assert_eq!(key2, initial_key, "Toggle castle back to non-zero failed");

        // Test combined flag (though toggle usually used with single flags)
        let combined_flag = Castling::WHITE_CASTLING; // WK | WQ
        let combined_key = castle_key(combined_flag);
        let mut key3 = initial_key;
        key3 ^= castle_key(combined_flag);
        assert_eq!(
            key3,
            initial_key ^ combined_key,
            "Toggle combined castle flag failed"
        );
        key3 ^= castle_key(combined_flag);
        assert_eq!(key3, initial_key, "Toggle combined castle flag back failed");
    }

    #[test]
    fn test_toggle_colour() {
        let s_key = side_key(); // Key indicating black to move

        // Test starting from zero
        let mut key1 = 0;
        key1 ^= side_key();
        assert_eq!(key1, s_key, "Toggle colour from zero failed");
        key1 ^= side_key(); // Toggle back
        assert_eq!(key1, 0, "Toggle colour back to zero failed");

        // Test starting from arbitrary value
        let initial_key = 0x9876543210FEDCBA;
        let mut key2 = initial_key;
        key2 ^= side_key();
        assert_eq!(
            key2,
            initial_key ^ s_key,
            "Toggle colour from non-zero failed"
        );
        key2 ^= side_key(); // Toggle back
        assert_eq!(key2, initial_key, "Toggle colour back to non-zero failed");
    }

    // --- End of tests for Key::toggle_* methods ---

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
            calculated_key,
            board.state().keys.key,
            "Stored key differs from calculated key"
        );
        // Note: Board::set likely doesn't update the key yet, so this assert might fail.
        // If Board::set *does* update the key, keep this assert. Otherwise, remove it.
        // assert_eq!(
        //     board.state.key, calculated_key,
        //     "Stored key differs from recalculated key for FEN: {}",
        //     fen
        // );
        calculated_key // Return the key calculated from scratch
    }

    #[test]
    fn test_startpos_key() {
        let key_start = get_key_from_fen(START_FEN);
        assert_ne!(key_start, 0, "Startpos key should not be zero");

        // Verify it's consistent
        let key_start_again = get_key_from_fen(START_FEN);
        assert_eq!(key_start, key_start_again, "Startpos key is not consistent");
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
        // Key changes from ALL to (ALL - BQ) = KQk
        let expected_key_kqk = key_kqkq ^ castle_key(Castling::ALL) ^ castle_key(Castling(7)); // 11 = 0111 (KQk)
        assert_eq!(
            key_kqk, expected_key_kqk,
            "Manual castling XOR (ALL -> KQk) did not match"
        );

        // Test XORing entire state
        // Key changes from ALL to NONE
        let expected_key_none = key_kqkq ^ castle_key(Castling::ALL) ^ castle_key(Castling::NONE); // XOR out ALL, XOR in NONE
        assert_eq!(
            key_none, expected_key_none,
            "Manual castling XOR (ALL -> NONE) did not match"
        );

        // Test XORing back
        // Key changes from NONE to ALL
        let expected_key_kqkq_again =
            key_none ^ castle_key(Castling::NONE) ^ castle_key(Castling::ALL); // XOR out NONE, XOR in ALL
        assert_eq!(
            key_kqkq, expected_key_kqkq_again,
            "XORing castling rights back did not restore key"
        );
    }

    #[test]
    fn test_enpassant_difference() {
        // Position after 1. e4
        let base_fen_str = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1";
        // Same position but with EP possible on e3 (after hypothetical previous move)
        let ep_fen_str = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"; // Add EP e3

        let key_base = get_key_from_fen(base_fen_str);
        let key_ep = get_key_from_fen(ep_fen_str);

        assert_ne!(key_base, key_ep, "Keys should differ only by EP square");
        // Check XORing EP key
        assert_eq!(
            key_ep,
            key_base ^ ep_key(File::FileE), // XOR in EP key for file E
            "Manual EP XOR did not match"
        );
        assert_eq!(
            key_base,
            key_ep ^ ep_key(File::FileE), // XOR out EP key for file E
            "XORing EP back did not restore key"
        );
    }

    #[test]
    fn test_kiwipete_key() {
        // Test against a known complex position
        let key_kiwi = get_key_from_fen(TRICKY_FEN);
        assert_ne!(key_kiwi, 0, "Kiwipete key should not be zero");

        // Compare against startpos
        let key_start = get_key_from_fen(START_FEN);
        assert_ne!(
            key_kiwi, key_start,
            "Kiwipete key should differ from startpos"
        );
    }

    #[test]
    fn test_pawn_key_calc() {
        let board_start = Board::from_fen(START_FEN).unwrap();
        let pawn_key_start = board_start.calc_pawn_key();

        let mut expected_key = 0;
        for sq in Square::iter() {
            if let Some(p) = board_start.on(sq) {
                if p.pt() == PieceType::Pawn {
                    expected_key ^= piece_key(p, sq);
                }
            }
        }
        assert_eq!(pawn_key_start, expected_key, "Startpos pawn key mismatch");
        assert_ne!(pawn_key_start, 0, "Startpos pawn key is zero");

        // Position with fewer pawns
        let fen_fewer_pawns = "rnbqkbnr/pp1ppppp/8/8/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1";
        let board_fewer = Board::from_fen(fen_fewer_pawns).unwrap();
        let pawn_key_fewer = board_fewer.calc_pawn_key();

        let mut expected_fewer_key = 0;
        for sq in Square::iter() {
            if let Some(p) = board_fewer.on(sq) {
                if p.pt() == PieceType::Pawn {
                    expected_fewer_key ^= piece_key(p, sq);
                }
            }
        }
        assert_eq!(
            pawn_key_fewer, expected_fewer_key,
            "Fewer pawns key mismatch"
        );
        assert_ne!(
            pawn_key_start, pawn_key_fewer,
            "Pawn keys should differ with different pawn structures"
        );

        // Test that only pawns are included
        let expected_diff =
            piece_key(Piece::WhitePawn, Square::E2) ^ piece_key(Piece::BlackPawn, Square::C7);
        assert_eq!(
            pawn_key_start ^ pawn_key_fewer,
            expected_diff,
            "Difference between pawn keys doesn't match removed pawns"
        );
    }
}
