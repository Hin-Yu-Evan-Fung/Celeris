//! build.rs - Compile-time generation script for Sophos chess engine.
//!
//! This script is executed by Cargo before compiling the main crate (`sophos`).
//! Its primary purpose is to generate the magic bitboard attack tables for
//! sliding pieces (rooks and bishops) and write them to a Rust source file
//! in the `OUT_DIR`. This avoids the costly computation of these tables
//! every time the engine starts up.
//!
//! The generated file (`magic_table.rs`) is then included in the main crate
//! using the `include!` macro (typically within the relevant move generation module).

// --- Import necessary items from the 'sophos' crate itself ---
// We declare 'sophos' as a build-dependency in Cargo.toml to access its types and functions.
// This requires organizing the necessary code (like types and the generation logic)
// into modules accessible from the build script. Here, we assume `core` and `utils`
// contain the required definitions.
mod buildutils;
mod core;
mod utils;

// Import specific items needed for generation.
use buildutils::generate_file;

fn main() -> std::io::Result<()> {
    generate_file()?;
    Ok(())
}
