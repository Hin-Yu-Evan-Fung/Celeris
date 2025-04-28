// Core module exports

// Board representation submodules
mod bitboard;
mod moves;
mod piece;
mod square;
mod types;

// Re-export common types for easier access
pub use bitboard::Bitboard;
pub use moves::{Move, MoveFlag};
pub use piece::{Piece, PieceType};
pub use square::{File, Rank, Square};
pub use types::{Castling, CastlingMask, Colour, Direction};
