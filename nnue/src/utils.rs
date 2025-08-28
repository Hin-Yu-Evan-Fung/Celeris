use std::ops::{Deref, DerefMut};

use chess::{Colour, Square};

use crate::params::{BUCKET_MAP, L1, NUM_OUTPUT_BUCKETS, QA};

/// Allocate a zero initialized boxed value over a generic type.
pub fn box_array<T>() -> Box<T> {
    unsafe {
        let layout = std::alloc::Layout::new::<T>();
        let ptr = std::alloc::alloc_zeroed(layout);
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        Box::from_raw(ptr.cast())
    }
}
#[inline]
pub fn screlu(x: i16) -> i32 {
    (x.clamp(0, QA as i16) as i32).pow(2)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C, align(64))]
pub struct Align64<T>(pub T);

impl<T, const SIZE: usize> Deref for Align64<[T; SIZE]> {
    type Target = [T; SIZE];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, const SIZE: usize> DerefMut for Align64<[T; SIZE]> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub const fn feature_index(
    c: usize,
    p: usize,
    mut wksq: Square,
    mut bksq: Square,
    s: usize,
) -> (usize, usize) {
    const F_BASE: usize = 768;
    const C_BASE: usize = 384;
    const P_BASE: usize = 64;

    let mut wflip: usize = 0;
    let mut bflip: usize = 56;

    if wksq.index() % 8 > 3 {
        wksq = wksq.flip_file();
        wflip ^= 7;
    }

    if bksq.index() % 8 > 3 {
        bksq = bksq.flip_file();
        bflip ^= 7;
    }

    let w_bucket_idx = input_bucket_index(wksq, Colour::White);
    let b_bucket_idx = input_bucket_index(bksq, Colour::Black);

    let w = w_bucket_idx * F_BASE + c * C_BASE + p * P_BASE + (s ^ wflip);
    let b = b_bucket_idx * F_BASE + (1 ^ c) * C_BASE + p * P_BASE + (s ^ bflip);

    (w * L1, b * L1)
}

pub const fn input_bucket_index(ksq: Square, c: Colour) -> usize {
    BUCKET_MAP[ksq.index() ^ (56 * c.index())]
}

pub const fn output_bucket_index(num_pieces: usize) -> usize {
    const DIV: usize = usize::div_ceil(32, NUM_OUTPUT_BUCKETS);
    (num_pieces - 2) / DIV
}
