//! # Module: `prng`
//!
//! This module provides a pseudo-random number generator (PRNG) based on the Xorshiro128+
//! algorithm. It is designed for use in non-cryptographic applications, such as chess engines,
//! where speed and statistical quality are important.
//!
//! ## Overview
//!
//! The PRNG in this module is a fast, high-quality generator that produces a sequence of
//! pseudo-random numbers. It is based on the Xorshiro128+ algorithm, which is known for its
//! speed and good statistical properties. The PRNG can be seeded with a 64-bit value, allowing
//! for reproducible sequences of random numbers.
//!
//! ## Key Features
//!
//! - **Fast Execution**: The Xorshiro128+ algorithm is computationally efficient, making it
//!   suitable for performance-critical applications.
//! - **High-Quality Randomness**: The generated numbers have good statistical properties,
//!   making them suitable for a wide range of applications.
//! - **Deterministic Output**: When seeded with the same value, the PRNG will produce the
//!   same sequence of random numbers. This is useful for testing and debugging.
//! - **Sparse Random Numbers**: The module provides a method for generating sparse random
//!   numbers, which have fewer bits set on average. This is useful for applications that
//!   require random bit patterns with low density.
//!
//! ## Usage
//!
//! The `PRNG` struct is the main component of this module. It can be created with a specific
//! seed or with a default seed. The `random_u64` method generates a random 64-bit unsigned
//! integer, and the `random_sparse_u64` method generates a sparse random 64-bit unsigned
//! integer.
//!
//! ## Implementation Details
//!
//! The PRNG is based on the Xorshiro256++ algorithm from [the PRNG page by Sebastiano Vigna](https://prng.di.unimi.it/xoshiro256plusplus.c).
//! The internal state of the PRNG is stored in four 64-bit unsigned integers. The `random_u64`
//! method updates the internal state and returns a new random number. The `random_sparse_u

/// # Pseudo-Random Number Generator (PRNG)
///
/// A fast, high-quality pseudo-random number generator based on the Xorshiro128+ algorithm.
/// This PRNG is designed for use in non-cryptographic applications like chess engines,
/// where speed and statistical quality are important.
///
/// ## Features
/// - Fast execution with minimal state
/// - Good statistical properties for chess-related randomization
/// - Deterministic output sequence for a given seed (useful for reproducible tests)
/// - Methods for generating both uniform and sparse random 64-bit values
///
/// ## Implementation
/// This implementation is based on the Xorshiro256++ algorithm from
/// [the PRNG page by Sebastiano Vigna](https://prng.di.unimi.it/xoshiro256plusplus.c).
///
/// ## Example Usage
/// ```rust, no_run
/// // Create a PRNG with default seed
/// let mut prng = PRNG::default();
///
/// // Generate a random 64-bit value
/// let random_value = prng.random_u64();
///
/// // Generate a sparse random 64-bit value (with fewer bits set)
/// let sparse_value = prng.random_sparse_u64();
///
/// // Create a PRNG with a specific seed for reproducible sequences
/// let mut seeded_prng = PRNG::new(0x123456789ABCDEF);
/// ```
pub(crate) struct PRNG {
    s: (u64, u64, u64, u64),
}

impl PRNG {
    /// Creates a new PRNG with the given seed.
    ///
    /// The seed is a 64-bit integer that initializes the PRNG's internal state.
    /// The same seed will always produce the same sequence of random numbers.
    ///
    /// # Arguments
    /// * `seed` - A 64-bit unsigned integer used to initialize the PRNG
    ///
    /// # Returns
    /// A new PRNG instance initialized with the given seed
    ///
    /// # Example
    /// ```rust,no_run
    /// let mut prng = PRNG::new(0x123456789ABCDEF);
    /// ```
    pub fn new(seed: u64) -> Self {
        let s0 = seed;
        let s1 = seed.wrapping_mul(2);
        let s2 = seed.wrapping_div(5);
        let s3 = seed.wrapping_add(seed.wrapping_div(2));

        PRNG {
            s: (s0, s1, s2, s3),
        }
    }

    /// Generates a random 64-bit unsigned integer.
    ///
    /// Implements the Xorshiro256++ algorithm, which is a fast, high-quality
    /// PRNG suitable for non-cryptographic applications.
    ///
    /// # Returns
    /// A random 64-bit unsigned integer with uniform distribution
    ///
    /// # Example
    /// ```rust, no_run
    /// let mut prng = PRNG::default();
    /// let random_value = prng.random_u64();
    /// ```
    #[inline]
    pub fn random_u64(&mut self) -> u64 {
        let t = self.s.1 << 17;
        self.s.2 ^= self.s.0;
        self.s.3 ^= self.s.1;
        self.s.1 ^= self.s.2;
        self.s.0 ^= self.s.3;
        self.s.2 ^= t;
        self.s.3 = self.s.3.rotate_left(45);

        self.s.0
    }

    /// Generates a sparse random 64-bit unsigned integer.
    ///
    /// This method produces a random value with fewer bits set on average
    /// by performing an AND operation on three randomly generated values.
    /// This is useful for generating sparser bitboards or test positions
    /// with fewer pieces.
    ///
    /// # Returns
    /// A sparse random 64-bit unsigned integer
    ///
    /// # Example
    /// ```rust, no_run
    /// let mut prng = PRNG::default();
    /// let sparse_value = prng.random_sparse_u64();
    /// ```
    #[inline]
    pub fn random_sparse_u64(&mut self) -> u64 {
        self.random_u64() & self.random_u64() & self.random_u64()
    }
}

impl Default for PRNG {
    /// Creates a new PRNG with a default seed.
    ///
    /// The default seed is a predetermined value that provides good
    /// randomization properties for chess-related applications.
    ///
    /// # Returns
    /// A new PRNG instance with the default seed
    ///
    /// # Example
    /// ```rust, no_run
    /// let mut prng = PRNG::default();
    /// ```
    fn default() -> Self {
        PRNG::new(0x6B51FF299F6A3AEE)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Bitboard;
    use crate::core::Square;

    fn random_bitboard(rng: &mut PRNG) -> Bitboard {
        Bitboard(rng.random_u64())
    }

    #[test]
    fn test_prng_sequence() {
        let mut prng = PRNG::new(12345);

        // With a fixed seed, we should get a deterministic sequence
        let first_sequence = (0..5).map(|_| prng.random_u64()).collect::<Vec<_>>();

        // Reset with the same seed
        let mut prng = PRNG::new(12345);
        let second_sequence = (0..5).map(|_| prng.random_u64()).collect::<Vec<_>>();

        // Both sequences should be identical
        assert_eq!(first_sequence, second_sequence);
    }

    #[test]
    fn test_default_seed() {
        let mut prng1 = PRNG::default();
        let mut prng2 = PRNG::default();

        // Two default PRNGs should produce the same first value
        assert_eq!(prng1.random_u64(), prng2.random_u64());
    }

    #[test]
    fn test_sparse_distribution() {
        let mut prng = PRNG::default();

        // Regular random_u64 should have ~32 bits set on average
        let mut regular_bits_count = 0;
        // Sparse random_u64 should have fewer bits set
        let mut sparse_bits_count = 0;

        for _ in 0..1000 {
            let regular = prng.random_u64();
            let sparse = prng.random_sparse_u64();

            regular_bits_count += regular.count_ones();
            sparse_bits_count += sparse.count_ones();
        }

        // Sparse should have fewer bits set on average
        assert!(sparse_bits_count < regular_bits_count / 2);
    }

    #[test]
    fn test_prng() {
        let mut prng = PRNG::default();
        for _ in 0..10 {
            println!("{}", prng.random_u64());
        }
    }

    #[test]
    fn check_bit_displacement() {
        let mut seeder = PRNG::default();
        let mut acc = [0u32; 64];

        for _ in 0..100 {
            let mut prng = PRNG::new(seeder.random_u64());
            for _ in 0..10000 {
                let bb = random_bitboard(&mut prng);
                for sq in Square::iter() {
                    if bb.get(sq) {
                        acc[sq as usize] += 1;
                    }
                }
            }
        }

        let max = *acc.iter().max().unwrap();
        for (i, m) in acc.iter_mut().enumerate() {
            *m *= 100;
            *m /= max;
            assert!(
                *m > 98,
                "Bitboard {} is not uniformly distributed: {}",
                i,
                *m
            );
        }

        let sum: u32 = acc.iter().sum();
        assert!(
            sum > 98 * 64,
            "Bitboard distribution is not uniform: {}",
            sum
        );
    }
}
