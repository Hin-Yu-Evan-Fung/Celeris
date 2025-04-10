// Core module exports

// Board representation submodules
pub mod bitboard;
pub mod piece;
pub mod square;
pub mod types;

// Re-export common types for easier access
pub use bitboard::Bitboard;
pub use piece::{Piece, PieceType};
pub use square::{File, Rank, Square};
pub use types::{Castling, Colour, Direction};
