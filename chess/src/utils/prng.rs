pub struct PRNG {
    s: (u64, u64, u64, u64),
}

impl PRNG {
    pub const fn new(seed: u64) -> Self {
        let s0 = seed;
        let s1 = seed.wrapping_mul(2);
        let s2 = seed.wrapping_div(5);
        let s3 = seed.wrapping_add(seed.wrapping_div(2));

        PRNG {
            s: (s0, s1, s2, s3),
        }
    }

    #[inline]
    pub const fn random_u64(&mut self) -> u64 {
        let t = self.s.1 << 17;
        self.s.2 ^= self.s.0;
        self.s.3 ^= self.s.1;
        self.s.1 ^= self.s.2;
        self.s.0 ^= self.s.3;
        self.s.2 ^= t;
        self.s.3 = self.s.3.rotate_left(45);

        self.s.0
    }

    #[inline]
    pub const fn random_sparse_u64(&mut self) -> u64 {
        self.random_u64() & self.random_u64() & self.random_u64()
    }
}

impl Default for PRNG {
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

        let first_sequence = (0..5).map(|_| prng.random_u64()).collect::<Vec<_>>();

        let mut prng = PRNG::new(12345);
        let second_sequence = (0..5).map(|_| prng.random_u64()).collect::<Vec<_>>();

        assert_eq!(first_sequence, second_sequence);
    }

    #[test]
    fn test_default_seed() {
        let mut prng1 = PRNG::default();
        let mut prng2 = PRNG::default();

        assert_eq!(prng1.random_u64(), prng2.random_u64());
    }

    #[test]
    fn test_sparse_distribution() {
        let mut prng = PRNG::default();

        let mut regular_bits_count = 0;

        let mut sparse_bits_count = 0;

        for _ in 0..1000 {
            let regular = prng.random_u64();
            let sparse = prng.random_sparse_u64();

            regular_bits_count += regular.count_ones();
            sparse_bits_count += sparse.count_ones();
        }

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
                    if bb.contains(sq) {
                        acc[sq.index()] += 1;
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
