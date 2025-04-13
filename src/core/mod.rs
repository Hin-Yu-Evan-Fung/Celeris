// Core module exports

// Board representation submodules
pub mod bitboard;
pub mod board;
pub mod errors;
pub mod moves;
pub mod piece;
pub mod square;
pub mod types;

// Re-export common types for easier access
pub use bitboard::Bitboard;
pub use board::fen;
pub use board::{Board, BoardState};
pub use moves::{Move, MoveFlag};
pub use piece::{Piece, PieceType};
pub use square::{File, Rank, Square};
pub use types::{Castling, Colour, Direction};
