use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

use super::magic_table_gen::{SliderAttackTable, gen_slider_attacks};
use crate::core::*;

// Note: We don't need to import the LazyLock tables themselves here, as those are
// meant for runtime initialization if *not* using build.rs. We call the
// generation function `gen_slider_attacks` directly.

/// The required size for the bishop attack table array.
/// Determined empirically or through analysis of magic bitboard generation.
const BISHOP_TABLE_SIZE: usize = 0x1480; // 5248 entries
/// The required size for the rook attack table array.
/// Determined empirically or through analysis of magic bitboard generation.
const ROOK_TABLE_SIZE: usize = 0x19000; // 102400 entries

// --- Helper Functions for Formatting Data as Rust Code ---

/// Formats a slice of `Bitboard`s into a Rust `const` array string.
///
/// # Arguments
/// * `name` - The desired name for the generated `const` array.
/// * `data` - A slice containing the `Bitboard` data.
///
/// # Returns
/// A `String` containing the Rust code definition for the constant array.
/// Bitboards are formatted in hexadecimal for consistency.
pub fn format_bitboard_array<const N: usize>(name: &str, data: &[Bitboard; N]) -> String {
    let items = data
        .iter()
        // Use hexadecimal format for better readability/consistency with magic numbers.
        .map(|bb| format!("Bitboard({:#X})", bb.0))
        .collect::<Vec<_>>()
        .join(",\n    ");
    // Define as a simple `const`. Visibility is handled by the module where
    // the generated file is included.
    format!("const {}: [Bitboard; {}] = [\n    {}\n];\n", name, N, items)
}

/// Formats a slice of `u64`s (typically magic numbers) into a Rust `const` array string.
///
/// # Arguments
/// * `name` - The desired name for the generated `const` array.
/// * `data` - A slice containing the `u64` data.
///
/// # Returns
/// A `String` containing the Rust code definition for the constant array.
/// Numbers are formatted in hexadecimal.
/// Note: Uses `pub(super)` visibility, assuming the generated file is included
/// within a private submodule. Adjust if needed.
pub fn format_u64_array<const N: usize>(name: &str, data: &[u64; N]) -> String {
    let items = data
        .iter()
        // Use hexadecimal format for consistency.
        .map(|&num| format!("{:#X}", num))
        .collect::<Vec<_>>()
        .join(",\n    ");
    // Using pub(super) allows the constants to be visible within the parent module
    // where the `include!(...)` likely resides, but not outside of it.
    format!(
        "pub(super) const {}: [u64; {}] = [\n    {}\n];\n",
        name, N, items
    )
}

/// Helper function specifically for formatting the `SliderAttackTable.table` field.
///
/// This function takes the *dereferenced* slice `&[Bitboard; N]` from the `Box`
/// holding the attack table data.
///
/// # Arguments
/// * `name` - The desired name for the generated `const` array (e.g., "BISHOP_TABLE").
/// * `data` - A slice representing the attack table data.
///
/// # Returns
/// A `String` containing the Rust code definition for the constant array.
pub fn format_slider_attack_table<const N: usize>(
    name: &str,
    data: &[Bitboard; N], // Note: Takes &[Bitboard; N], not Box<...>
) -> String {
    // Internally uses the same formatting as any other Bitboard array.
    format_bitboard_array(name, data)
}

pub fn generate_file() -> std::io::Result<()> {
    // --- Cargo Instructions ---
    // Tell Cargo to rerun this script only if build.rs itself changes.
    println!("cargo:rerun-if-changed=build.rs");
    // Tell Cargo to rerun this script if the source files containing the
    // generation logic or related types change. This ensures the tables
    // are regenerated if the underlying algorithms are modified.
    // Adjust these paths based on your actual project structure.
    println!("cargo:rerun-if-changed=src/core/types.rs"); // Assuming types like Bitboard are here
    println!("cargo:rerun-if-changed=src/utils/magic_table_gen.rs"); // The generation logic itself

    println!("[build.rs] Starting magic attack table generation...");

    // --- Generate Magic Bitboard Tables ---
    // Call the generation function (defined in src/utils/magic_table_gen.rs or similar)
    // for both bishop and rook piece types. This is the computationally intensive part.
    let bishop_table_data: SliderAttackTable<BISHOP_TABLE_SIZE> =
        gen_slider_attacks::<BISHOP_TABLE_SIZE>(PieceType::Bishop);
    println!("[build.rs] Generated Bishop Table data.");

    let rook_table_data: SliderAttackTable<ROOK_TABLE_SIZE> =
        gen_slider_attacks::<ROOK_TABLE_SIZE>(PieceType::Rook);
    println!("[build.rs] Generated Rook Table data.");

    // --- Extract Magic Numbers ---
    // The `gen_slider_attacks` function likely returns a struct containing both the
    // attack table and the magic numbers used. Extract the magic numbers separately.
    let bishop_magic_nums: [u64; Square::NUM] = bishop_table_data
        .magic // Assuming `magic` is the field holding MagicEntry structs or similar
        .iter()
        .map(|m| m.magic) // Assuming each entry `m` has a `magic` field of type u64
        .collect::<Vec<_>>()
        .try_into() // Convert Vec<u64> into [u64; 64]
        .expect("Failed to collect bishop magic numbers into array"); // Handle potential error

    let rook_magic_nums: [u64; Square::NUM] = rook_table_data
        .magic
        .iter()
        .map(|m| m.magic)
        .collect::<Vec<_>>()
        .try_into() // Convert Vec<u64> into [u64; 64]
        .expect("Failed to collect rook magic numbers into array"); // Handle potential error

    // --- Format Generated Data as Rust Code ---
    // Build a string containing the Rust source code for the constants.
    let mut generated_code = String::new();

    // Add necessary header comments or module attributes if desired

    // Define the table size constants needed by the array types in the generated code.
    generated_code.push_str(&format!(
        "pub(super) const BISHOP_TABLE_SIZE: usize = {};\n", // Use pub(super) for consistency
        BISHOP_TABLE_SIZE
    ));
    generated_code.push_str(&format!(
        "pub(super) const ROOK_TABLE_SIZE: usize = {};\n\n", // Use pub(super) for consistency
        ROOK_TABLE_SIZE
    ));

    // Format the magic number arrays.
    generated_code.push_str(&format_u64_array("BISHOP_MAGIC_NUMS", &bishop_magic_nums));
    generated_code.push_str("\n"); // Add spacing
    generated_code.push_str(&format_u64_array("ROOK_MAGIC_NUMS", &rook_magic_nums));
    generated_code.push_str("\n"); // Add spacing

    // Format the computed attack tables.
    // We need to dereference the Box (`&*`) to pass a slice `&[Bitboard; N]`
    // to the formatting function.
    generated_code.push_str(&format_slider_attack_table(
        "BISHOP_TABLE",
        &*bishop_table_data.table, // Deref Box<[T; N]> to get &[T; N]
    ));
    generated_code.push_str("\n"); // Add spacing
    generated_code.push_str(&format_slider_attack_table(
        "ROOK_TABLE",
        &*rook_table_data.table, // Deref Box<[T; N]> to get &[T; N]
    ));

    // --- Write Generated Code to File ---
    // Get the output directory path from the `OUT_DIR` environment variable set by Cargo.
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR environment variable not set"));
    // Define the full path for the generated Rust source file.
    let dest_path = out_dir.join("magic_table.rs");

    println!("[build.rs] Writing generated tables to: {:?}", dest_path);

    // Create (or overwrite) the destination file and write the generated code string into it.
    // Using BufWriter for potentially better performance with large amounts of data.
    let mut file = BufWriter::new(File::create(&dest_path)?);
    write!(file, "{}", generated_code)?;

    // Ensure the buffer is flushed and file is written.
    file.flush()?;

    println!("[build.rs] Successfully wrote generated tables.");

    // Return Ok to indicate successful execution of the build script.
    Ok(())
}
