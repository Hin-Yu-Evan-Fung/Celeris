//! # Module: `board`
//!
//! This module defines the core data structures and functionalities for representing a chess board
//! and managing its state. It includes the `Board` struct, which encapsulates the entire state of
//! a chess game, and the `BoardState` struct, which stores the state of the board at a specific
//! point in time.
//!
//! ## Overview
//!
//! The `board` module is a fundamental part of the chess engine, providing the basic building
//! blocks for representing the game state, managing piece positions, handling move history, and
//! enforcing game rules. It uses bitboards for efficient storage and manipulation of piece
//! locations and provides methods for accessing and modifying the board state.
//!
//! ## Key Components
//!
//! - **`Board`**: A struct representing the complete state of a chess game.
//!   - Contains bitboards for piece positions, occupied squares, and side to move.
//!   - Manages move counters, castling rights, en passant square, and game history.
//!   - Provides methods for accessing piece positions, occupied squares, and other board state information.
//!   - Implements `Display` for printing the board to the console.
//! - **`BoardState`**: A struct representing the state of the board at a specific point in time.
//!   - Stores information about castling rights, en passant square, captured piece, and move counters.
//!   - Includes Zobrist keys for position hashing and repetition detection.
//!   - Contains bitboard masks for move generation and legality checks.
//!
//! ## Functionality
//!
//! - **Board Creation**: `Board::new()` creates an empty board, while `Board::default()` creates a board with the standard starting position.
//! - **State Management**: `Board::store_state()` and `Board::restore_state()` manage the move history, allowing for undoing moves.
//! - **Piece Access**: `Board::on()` retrieves the piece on a given square.
//! - **Bitboard Access**: `Board::piecetype_bb()`, `Board::occupied_bb()`, `Board::all_occupied_bb()

use crate::core::*;
use fen::START_FEN;

/******************************************\
|==========================================|
|                Constants                 |
|==========================================|
\******************************************/

const MAX_DEPTH: usize = 256;

/******************************************\
|==========================================|
|               Board State                |
|==========================================|
\******************************************/

/// # Board State (Used for move generation and history)
///
/// Stores the state of the board at a given point in time.
/// This is used to restore the board to a previous state after a move has been made (unmake move)
/// and to check for game rules like repetitions. It also caches information useful for move generation.
///
/// Caches the variables that cannot be rolled back in a predictable pattern, for example, the previously captured piece has to be stored or else it will be lost to history
///
/// ## Fields
/// - `enpassant`: The en passant square, if any, available on this turn. `None` if no en passant capture is possible.
/// - `castle`: The castling rights (`Castling`) available *before* the move that led to this state.
/// - `key`: The Zobrist key representing the board position (excluding pawn structure and move counters). Used for repetition detection.
/// - `pawn_key`: The Zobrist key specifically for the pawn structure. Potentially useful for pawn structure evaluation.
/// - `captured`: The `Piece` captured on the move leading to this state. `Piece::None` if no capture occurred. Essential for `unmake_move`.
/// - `repetitions`: The number of times this exact position (`key`) has been repeated in the game history up to this point.
/// - `fifty_move`: Counter for the fifty-move rule (number of plies since the last pawn move or capture).
/// - `check_mask`: A bitboard indicating squares that block or capture checking pieces. If not in check, this is typically all squares (`!0`).
/// - `diag_pin`: A bitboard representing squares occupied by pieces pinned diagonally to their king.
/// - `hv_pin`: A bitboard representing squares occupied by pieces pinned horizontally or vertically to their king.
/// - `king_ban`: A bitboard representing squares the king cannot move to (attacked squares or squares occupied by friendly pieces).
/// - `king_attacks`: A bitboard representing the squares attacked by the friendly king.
/// - `available`: A bitboard representing all squares not occupied by the side to move (potential destinations, excluding captures).
/// - `enpassant_pin`: A boolean indicating if the pawn performing an en passant capture is pinned to the king, making the en passant illegal.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct BoardState {
    // --- Board State Variables -- //
    /// Number of times the current position (by `key`) has occurred in the game history.
    repetitions: u8,
    /// Counter for the fifty-move rule (plies since last pawn move or capture).
    fifty_move: u8,
    /// The piece that was captured on the move leading to this state. `Piece::None` if no capture.
    captured: Option<Piece>, // Assuming Piece::None exists
    /// The square where an en passant capture is possible, if any. `None` otherwise.
    enpassant: Option<Square>,
    /// Castling rights (`Castling`) available *before* the move leading to this state.
    castle: Castling,
    /// Zobrist keys for the current position
    key: Key,
    /// Zobrist Key for the current position (Only for pawns)
    pawn_key: Key,

    // --- Move generation masks ---
    /// Bitboard mask: squares that block check or capture the checking piece(s). If not in check, all squares (`!0`).
    check_mask: Bitboard,
    /// Bitboard mask: squares occupied by pieces pinned diagonally to their king.
    diag_pin: Bitboard,
    /// Bitboard mask: squares occupied by pieces pinned horizontally or vertically to their king.
    hv_pin: Bitboard,
    /// Bitboard mask: squares the king cannot move to (attacked or occupied by friendly pieces).
    king_ban: Bitboard,
    /// Flag indicating if the pawn that could capture en passant is pinned, making the move illegal.
    enpassant_pin: bool,
}

/******************************************\
|==========================================|
|                  Board                   |
|==========================================|
\******************************************/

/// # Chess Board Representation
///
/// Represents the complete state of a chess game at a specific point in time.
/// It encapsulates piece positions, castling rights, the side to move, move counters,
/// and a history of previous states for undoing moves and rule checking (like repetitions).
///
/// This struct is central to the chess engine's logic, providing the foundation for
/// move generation, move execution, and game state evaluation.
///
/// ## Representation
/// The board primarily uses `Bitboard`s for efficient storage and manipulation of piece locations.
///
/// ## Fields
/// - `pieces`: An array of `Bitboard`s, indexed by `PieceType`. Each bitboard (`pieces[PieceType::Pawn as usize]`)
///   stores the locations of all pieces of that specific type, regardless of colour.
/// - `occupied`: An array of `Bitboard`s, indexed by `Colour`. Each bitboard (`occupied[Colour::White as usize]`)
///   stores the locations of all pieces belonging to that specific colour. `occupied[0] | occupied[1]` gives all occupied squares.
/// - `side_to_move`: The `Colour` (White or Black) whose turn it is to move.
/// - `half_moves`: Counts the number of half-moves (plies) since the last capture or pawn advance.
///   Used for the fifty-move rule. It's incremented after each move and reset to 0 upon a capture or pawn move.
/// - `state`: The current `BoardState` of the board. This includes en passant square, castling rights,
///   captured piece, Zobrist keys, and move counters.
/// - `history`: A `Vec<BoardState>` acting as a stack. Each time a move is made, the current `BoardState`
///   (containing castling rights, en passant square, captured piece, keys, etc., *before* the move)
///   is pushed onto this stack. This allows for efficient `unmake_move` operations and tracking game history
///   for rules like three-fold repetition.
#[derive(Debug)]
pub struct Board {
    /// Array representing the board, where each element corresponds to a square.
    /// `board[Square::A1 as usize]` holds the `Piece` on A1.
    /// `Piece::None` indicates an empty square.
    board: [Option<Piece>; Square::NUM],

    /// Bitboards for each piece type (Pawn, Knight, Bishop, Rook, Queen, King).
    /// `pieces[PieceType::Pawn as usize]` holds a bitboard of all pawns (both colours).
    pieces: [Bitboard; PieceType::NUM],

    /// Bitboards for all pieces of each colour.
    /// `occupied[Colour::White as usize]` holds all white pieces.
    /// `occupied[Colour::Black as usize]` holds all black pieces.
    occupied: [Bitboard; Colour::NUM],

    /// Counts the number of half-moves (plies) since the last capture or pawn advance.
    /// Used for the fifty-move rule. Reset to 0 on capture or pawn move.
    half_moves: u16, // Renamed from fifty_move in BoardState for clarity, common practice

    /// Indicates which player's turn it is (White or Black).
    side_to_move: Colour,

    /// Current board state
    state: BoardState,
    // /// A stack-like structure storing previous board states (`BoardState`).
    // /// Used to undo moves (`unmake_move`) and track game history (e.g., for repetition checks).
    // history: [BoardState; MAX_DEPTH],
}

/******************************************\
|==========================================|
|           Basic Implementation           |
|==========================================|
\******************************************/

impl Board {
    /// # New Board
    ///
    /// - Creates a new empty board, initialising the history stack
    pub fn new() -> Board {
        Board {
            board: [None; Square::NUM],
            pieces: [Bitboard::EMPTY; PieceType::NUM],
            occupied: [Bitboard::EMPTY; Colour::NUM],
            side_to_move: Colour::White,
            half_moves: 0,
            state: BoardState::default(),
            // history: [BoardState::default(); MAX_DEPTH],
        }
    }

    /// # Default Board
    ///
    /// - Creates a new board with the starting position
    pub fn default() -> Board {
        let mut board = Board::new();
        board.set(START_FEN).unwrap();
        board
    }

    // /// # Restore State
    // ///
    // /// - Restores the board to the previous state
    // /// - Pops the last board state from the history stack
    // /// - Updates the board state
    // ///
    // /// ## Panics
    // ///
    // /// - Panics if the history stack is empty
    // ///
    // /// ## Arguments
    // ///
    // /// * `self` - The board to restore the state of
    // fn store_state(&mut self) {
    //     // Store the current board state in the history stack
    //     self.history[self.half_moves as usize] = self.state;
    //     self.half_moves += 1;
    // }

    // /// # Restore State
    // ///
    // /// - Restores the board to the previous state
    // /// - Pops the last board state from the history stack
    // /// - Updates the board state
    // ///
    // /// ## Panics
    // ///
    // /// - Panics if the history stack is empty
    // ///
    // /// ## Arguments
    // ///
    // /// * `self` - The board to restore the state of
    // fn restore_state(&mut self) {
    //     // Pop the last board state from the history stack
    //     self.state = self.history[self.half_moves as usize];
    //     self.half_moves -= 1;
    // }

    /// # Get Piece on Square
    ///
    /// ## Arguments
    ///
    // / * `square` - The square to get the piece from    ///
    /// ## Returns
    ///
    /// * `Option<Piece>` - The piece on the square, or `None` if the square is empty
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.on(Square::A1), Some(Piece::WhiteRook));
    /// assert_eq!(board.on(Square::A2), Some(Piece::WhitePawn));
    /// assert_eq!(board.on(Square::A3), None);
    /// ```
    pub const fn on(&self, square: Square) -> Option<Piece> {
        self.board[square as usize]
    }

    /// # Get Piece Type Bitboard
    ///
    /// ## Argument
    ///
    /// * `piecetype` - The piece type to get the bitboard of
    ///
    /// ## Returns
    ///
    /// * `Bitboard` - The bitboard of the piece type
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.piecetype_bb(PieceType::Pawn), Bitboard::from([
    ///     Square::A2, Square::B2, Square::C2, Square::D2, Square::E2, Square::F2, Square::G2, Square::H2,
    ///     Square::A7, Square::B7, Square::C7, Square::D7, Square::E7, Square::F7, Square::G7, Square::H7,
    /// ]));
    /// assert_eq!(board.piecetype_bb(PieceType::Rook), Bitboard::from([
    ///     Square::A1, Square::H1, Square::A8, Square::H8,
    /// ]));
    ///
    pub const fn piecetype_bb(&self, piecetype: PieceType) -> Bitboard {
        self.pieces[piecetype as usize]
    }

    /// # Get Occupied Bitboard
    ///
    /// ## Argument
    ///
    /// * `colour` - The colour to get the occupied bitboard of
    ///     ///
    /// ## Returns
    ///
    /// * `Bitboard` - The bitboard of the occupied squares of the colour
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.occupied_bb(Colour::White), Bitboard::from([
    ///     Square::A1, Square::B1, Square::C1, Square::D1, Square::E1, Square::F1, Square::G1, Square::H1,
    ///     Square::A2, Square::B2, Square::C2, Square::D2, Square::E2, Square::F2, Square::G2, Square::H2,
    /// ]));
    /// assert_eq!(board.occupied_bb(Colour::Black), Bitboard::from([
    ///     Square::A8, Square::B8, Square::C8, Square::D8, Square::E8, Square::F8, Square::G8, Square::H8,
    ///     Square::A7, Square::B7, Square::C7, Square::D7, Square::E7, Square::F7, Square::G7, Square::H7,
    /// ]));
    ///
    pub const fn occupied_bb(&self, colour: Colour) -> Bitboard {
        self.occupied[colour as usize]
    }

    /// # Get Total Occupied Bitboard
    ///
    /// ## Returns
    ///
    /// * `Bitboard` - The bitboard of the occupied squares
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.occupied_bb(Colour::White), Bitboard::from([
    ///     Square::A1, Square::B1, Square::C1, Square::D1, Square::E1, Square::F1, Square::G1, Square::H1,
    ///     Square::A2, Square::B2, Square::C2, Square::D2, Square::E2, Square::F2, Square::G2, Square::H2,
    /// ] | Bitboard::from([
    ///     Square::A8, Square::B8, Square::C8, Square::D8, Square::E8, Square::F8, Square::G8, Square::H8,
    ///     Square::A7, Square::B7, Square::C7, Square::D7, Square::E7, Square::F7, Square::G7, Square::H7,
    /// ]));
    /// ```
    pub const fn all_occupied_bb(&self) -> Bitboard {
        self.occupied_bb(Colour::White)
            .bitor(self.occupied_bb(Colour::Black))
    }

    /// # Get Piece Bitboard
    ///
    /// ## Argument
    ///
    /// * `piece` - The piece to get the bitboard of    ///
    /// ## Returns
    ///
    /// * `Bitboard` - The bitboard of the piece
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.piece_bb(Piece::WhitePawn), Bitboard::from([
    ///     Square::A2, Square::B2, Square::C2, Square::D2, Square::E2, Square::F2, Square::G2, Square::H2,
    /// ]));
    /// assert_eq!(board.piece_bb(Piece::BlackRook), Bitboard::from([
    ///     Square::A8, Square::H8,
    /// ]));
    ///
    pub const fn piece_bb(&self, piece: Piece) -> Bitboard {
        self.piecetype_bb(piece.piecetype())
            .bitand(self.occupied_bb(piece.colour()))
    }

    /// # Get Side To Move
    ///
    /// ## Returns
    ///
    /// * `Colour` - The colour of the side to move
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.side_to_move(), Colour::White);
    /// ```
    pub const fn side_to_move(&self) -> Colour {
        self.side_to_move
    }

    /// # Get half moves
    ///
    /// ## Returns
    ///
    /// * `u16` - The number of half moves since the last capture or pawn move
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.half_moves(), 0);
    /// ```
    pub const fn half_moves(&self) -> u16 {
        self.half_moves
    }

    /// # Get state
    ///
    /// ## Returns
    ///
    /// * `BoardState` - The current board state
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let board = Board::default();
    /// assert_eq!(board.state.castle, Castling::ALL);
    /// ```
    pub(crate) const fn state(&self) -> &BoardState {
        &self.state
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const SEPARATOR: &str = "\n     +---+---+---+---+---+---+---+---+";

        writeln!(f, "{}", SEPARATOR)?;

        for rank in Rank::iter().rev() {
            write!(f, " {}   |", rank as u8 + 1)?;

            for file in File::iter() {
                let square = Square::from_parts(file, rank);
                let cell = match self.on(square) {
                    Some(piece) => piece.to_string(),
                    None => " ".to_string(),
                };
                write!(f, " {} |", cell)?;
            }

            writeln!(f, "{}", SEPARATOR)?;
        }

        writeln!(f)?;
        writeln!(f, "       A   B   C   D   E   F   G   H")?;
        writeln!(f)?;
        writeln!(f, "Current Side: {:?}", self.side_to_move())?;
        writeln!(f, "Castling: {}", self.state.castle)?;
        writeln!(
            f,
            "En Passant Square: {}",
            match self.state.enpassant {
                Some(square) => square.to_string(),
                None => "None".to_string(),
            }
        )?;
        writeln!(f, "Half Move Clock: {}", self.state.fifty_move)?;
        writeln!(f, "Full Move: {}", self.half_moves / 2 + 1)?;
        writeln!(f, "Fen: {}", self.fen())?;

        Ok(())
    }
}

pub mod fen;
pub mod movement;
pub mod zobrist;

use zobrist::Key;
