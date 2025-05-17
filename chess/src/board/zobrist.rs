use super::Board;
use crate::core::*;
use crate::utils::PRNG;

/******************************************\
|==========================================|
|              Key Definition              |
|==========================================|
\******************************************/

/// Type alias for a Zobrist key, typically a 64-bit unsigned integer.
pub type Key = u64;

/******************************************\
|==========================================|
|                Key Bundle                |
|==========================================|
\******************************************/

/// A bundle of Zobrist keys representing the state of a chess board.
///
/// This includes the main board key, a key specific to pawn structure,
/// and keys for non-pawn material for each color. This can be useful
/// for more granular hash table lookups (e.g., pawn hash tables).
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct KeyBundle {
    /// The main Zobrist key for the entire board position.
    pub key: Key,
    /// A Zobrist key representing only the pawn structure.
    pub pawn_key: Key,
    /// Zobrist keys representing non-pawn material for each color.
    /// Indexed by `Colour::index()`.
    pub non_pawn_key: [Key; Colour::NUM],
}

impl KeyBundle {
    /// Toggles a piece on a square in the key bundle.
    ///
    /// Updates the main key, and conditionally the pawn_key or non_pawn_key
    /// based on the piece type.
    #[inline]
    pub fn toggle_piece(&mut self, piece: Piece, sq: Square) {
        if piece.pt() as u8 == PieceType::Pawn as u8 {
            self.pawn_key ^= piece_key(piece, sq);
        } else {
            self.non_pawn_key[piece.colour().index()] ^= piece_key(piece, sq);
        }
        self.key ^= piece_key(piece, sq);
    }

    /// Toggles a castling right in the main key.
    #[inline]
    pub fn toggle_castle(&mut self, flag: Castling) {
        self.key ^= castle_key(flag);
    }

    /// Toggles the side to move in the main key.
    #[inline]
    pub fn toggle_side(&mut self) {
        self.key ^= side_key();
    }

    /// Toggle the enpassant key in the main key.
    #[inline]
    pub fn toggle_ep(&mut self, file: File) {
        self.key ^= ep_key(file);
    }
}

/******************************************\
|==========================================|
|              Zobrist Table               |
|==========================================|
\******************************************/

/// Contains the precomputed random numbers for Zobrist hashing.
///
/// Each unique game state element (piece on a square, castling rights,
/// side to move, en passant target) has an associated random key.
#[derive(Debug)]
pub struct ZobristTable {
    /// Keys for each piece on each square: `pieces[piece_index][square_index]`.
    pub pieces: [[Key; Square::NUM]; Piece::NUM],
    /// Key for indicating the side to move (typically XORed if it's Black's turn).
    pub side_to_move: Key,
    /// Keys for each possible castling rights mask: `castling[castling_mask]`.
    pub castling: [Key; Castling::NUM],
    /// Keys for each possible en passant file: `enpassant[file_index]`.
    pub enpassant: [Key; File::NUM],
}

/// Global static instance of the ZobristTable, initialized at compile time.
///
/// This table provides the random keys used for Zobrist hashing throughout the application.
pub const ZOBRIST: ZobristTable = init_zobrist_table();

/******************************************\
|==========================================|
|              Initialisation              |
|==========================================|
\******************************************/

/// Initializes the ZobristTable with pseudo-random 64-bit numbers.
const fn init_zobrist_table() -> ZobristTable {
    let mut rng = PRNG::new(0xDEADBEEFCAFEBABE);

    let mut pieces = [[0; Square::NUM]; Piece::NUM];
    let mut castling = [0; Castling::NUM];
    let mut enpassant = [0; File::NUM];

    let mut i = 0;
    while i < Piece::NUM {
        let mut j = 0;
        while j < Square::NUM {
            pieces[i][j] = rng.random_u64();
            j += 1;
        }
        i += 1;
    }

    let side_to_move = rng.random_u64();

    i = 0;
    while i < Castling::NUM {
        castling[i] = rng.random_u64();
        i += 1;
    }

    i = 0;
    while i < File::NUM {
        enpassant[i] = rng.random_u64();
        i += 1;
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

/// Retrieves the Zobrist key for a specific piece on a specific square.
#[inline]
pub fn piece_key(piece: Piece, sq: Square) -> Key {
    unsafe {
        *ZOBRIST
            .pieces
            .get_unchecked(piece.index())
            .get_unchecked(sq.index())
    }
}

/// Retrieves the Zobrist key for the side to move.
/// This key is typically XORed into the hash if it's Black's turn.
#[inline]
pub fn side_key() -> Key {
    ZOBRIST.side_to_move
}

/// Retrieves the Zobrist key for a given set of castling rights.
#[inline]
pub fn castle_key(flag: Castling) -> Key {
    ZOBRIST.castling[flag.0 as usize]
}

/// Retrieves the Zobrist key for an en passant capture being possible on a given file.
#[inline]
pub fn ep_key(file: File) -> Key {
    unsafe { *ZOBRIST.enpassant.get_unchecked(file.index()) }
}

/******************************************\
|==========================================|
|            Board Implementation          |
|==========================================|
\******************************************/

impl Board {
    /// Calculate the main zobrist key of the board
    pub(crate) fn calc_key(&self) -> Key {
        let mut key = 0;

        // Loop through all squares and toggling keys for each piece and square pair
        for sq in Square::iter() {
            if let Some(piece) = self.on(sq) {
                key ^= piece_key(piece, sq);
            }
        }

        // Toggle the side key if the side to move is black
        if self.stm as u8 == Colour::Black as u8 {
            key ^= side_key();
        }

        // Toggle castling key according to the current castling rights
        key ^= castle_key(self.state.castle);

        // Toggle enpassant key based on the file of the enpassant pawn
        if let Some(ep_square) = self.state.enpassant {
            key ^= ep_key(ep_square.file());
        }

        key
    }

    /// Calculate the pawn key of the board
    pub(crate) fn calc_pawn_key(&self) -> Key {
        let mut key = 0;

        // Loop through all squares and toggling keys for each pawn and square pair
        for sq in Square::iter() {
            if let Some(piece) = self.on(sq) {
                if piece.pt() as u8 == PieceType::Pawn as u8 {
                    key ^= piece_key(piece, sq);
                }
            }
        }

        key
    }

    pub(crate) fn calc_non_pawn_key(&self) -> [Key; Colour::NUM] {
        let mut keys = [0; Colour::NUM];

        // Loop through all squares and toggling keys for each non-pawn and square pair
        for sq in Square::iter() {
            if let Some(piece) = self.on(sq) {
                if piece.pt() as u8 != PieceType::Pawn as u8 {
                    keys[piece.colour().index()] ^= piece_key(piece, sq);
                }
            }
        }

        keys
    }
}

/******************************************\
|==========================================|
|                Unit Tests                |
|==========================================|
\******************************************/

#[cfg(test)]
mod tests {

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
        k4 ^= k2;
        assert_eq!(k4, k1);
    }

    #[test]
    fn zobrist_table_init() {
        assert_ne!(piece_key(Piece::WhitePawn, Square::E4), 0);
        assert_ne!(piece_key(Piece::BlackKing, Square::G8), 0);
        assert_ne!(side_key(), 0);
        assert_ne!(castle_key(Castling::WK), 0);
        assert_ne!(ep_key(File::FileD), 0);

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

    #[test]
    fn test_toggle_piece() {
        let piece = Piece::WhiteKnight;
        let sq = Square::C3;
        let p_key = piece_key(piece, sq);

        let mut key1 = 0;
        key1 ^= piece_key(piece, sq);
        assert_eq!(key1, p_key, "Toggle piece from zero failed");
        key1 ^= piece_key(piece, sq);
        assert_eq!(key1, 0, "Toggle piece back to zero failed");

        let initial_key = 0xABCDEF1234567890;
        let mut key2 = initial_key;
        key2 ^= piece_key(piece, sq);
        assert_eq!(
            key2,
            initial_key ^ p_key,
            "Toggle piece from non-zero failed"
        );
        key2 ^= piece_key(piece, sq);
        assert_eq!(key2, initial_key, "Toggle piece back to non-zero failed");
    }

    #[test]
    fn test_toggle_castle() {
        let flag = Castling::BQ;
        let c_key = castle_key(flag);

        let mut key1 = 0;
        key1 ^= castle_key(flag);
        assert_eq!(key1, c_key, "Toggle castle from zero failed");
        key1 ^= castle_key(flag);
        assert_eq!(key1, 0, "Toggle castle back to zero failed");

        let initial_key = 0x1122334455667788;
        let mut key2 = initial_key;
        key2 ^= castle_key(flag);
        assert_eq!(
            key2,
            initial_key ^ c_key,
            "Toggle castle from non-zero failed"
        );
        key2 ^= castle_key(flag);
        assert_eq!(key2, initial_key, "Toggle castle back to non-zero failed");

        let combined_flag = Castling::WHITE_CASTLING;
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
        let s_key = side_key();

        let mut key1 = 0;
        key1 ^= side_key();
        assert_eq!(key1, s_key, "Toggle colour from zero failed");
        key1 ^= side_key();
        assert_eq!(key1, 0, "Toggle colour back to zero failed");

        let initial_key = 0x9876543210FEDCBA;
        let mut key2 = initial_key;
        key2 ^= side_key();
        assert_eq!(
            key2,
            initial_key ^ s_key,
            "Toggle colour from non-zero failed"
        );
        key2 ^= side_key();
        assert_eq!(key2, initial_key, "Toggle colour back to non-zero failed");
    }

    fn get_key_from_fen(fen: &str) -> Key {
        let mut board = Board::new();

        board
            .set(fen)
            .expect(&format!("Test FEN failed to parse: {}", fen));

        let calculated_key = board.calc_key();

        assert_eq!(
            calculated_key, board.state.keys.key,
            "Stored key differs from calculated key"
        );

        calculated_key
    }

    #[test]
    fn test_startpos_key() {
        let key_start = get_key_from_fen(START_FEN);
        assert_ne!(key_start, 0, "Startpos key should not be zero");

        let key_start_again = get_key_from_fen(START_FEN);
        assert_eq!(key_start, key_start_again, "Startpos key is not consistent");
    }

    #[test]
    fn test_piece_difference() {
        let key_start = get_key_from_fen(START_FEN);

        let key_nc3 =
            get_key_from_fen("rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 0 1");
        assert_ne!(
            key_start, key_nc3,
            "Keys should differ based on piece placement"
        );

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

        let expected_key_kqk = key_kqkq ^ castle_key(Castling::ALL) ^ castle_key(Castling(7));
        assert_eq!(
            key_kqk, expected_key_kqk,
            "Manual castling XOR (ALL -> KQk) did not match"
        );

        let expected_key_none = key_kqkq ^ castle_key(Castling::ALL) ^ castle_key(Castling::NONE);
        assert_eq!(
            key_none, expected_key_none,
            "Manual castling XOR (ALL -> NONE) did not match"
        );

        let expected_key_kqkq_again =
            key_none ^ castle_key(Castling::NONE) ^ castle_key(Castling::ALL);
        assert_eq!(
            key_kqkq, expected_key_kqkq_again,
            "XORing castling rights back did not restore key"
        );
    }

    #[test]
    fn test_enpassant_difference() {
        let base_fen_str = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1";

        let ep_fen_str = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";

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
        let key_kiwi = get_key_from_fen(TRICKY_FEN);
        assert_ne!(key_kiwi, 0, "Kiwipete key should not be zero");

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

        let expected_diff =
            piece_key(Piece::WhitePawn, Square::E2) ^ piece_key(Piece::BlackPawn, Square::C7);
        assert_eq!(
            pawn_key_start ^ pawn_key_fewer,
            expected_diff,
            "Difference between pawn keys doesn't match removed pawns"
        );
    }
}
