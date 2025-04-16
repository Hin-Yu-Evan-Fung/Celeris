/// # Module: `utils`
///
/// This module provides a collection of utility functions and data structures used throughout the
/// chess engine. It includes tools for random number generation, magic number generation, and
/// other miscellaneous utilities.
///
/// ## Submodules
///
/// - `magic_gen`: Contains functions for generating magic numbers and attack tables for sliding
///   pieces.
/// - `misc`: Provides miscellaneous utility functions, such as calculating absolute differences.
/// - `prng`: Implements a pseudo-random number generator (PRNG) for use in various parts of the
///   engine.
///
/// ## Usage
///
/// The `utils` module is used internally by other modules in the chess engine to perform various
/// tasks. The functions and data structures in this module are not intended to be used directly
/// by external code.
///
/// ## Implementation Details
///
/// The `utils` module is designed to be efficient and reusable. It uses techniques such as
/// bitwise operations and lookup tables to optimize performance.
pub(crate) mod misc;
pub(crate) mod prng;

pub(crate) use misc::abs_diff;
pub(crate) use prng::PRNG;
