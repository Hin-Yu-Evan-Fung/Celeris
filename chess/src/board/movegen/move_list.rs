use std::mem::MaybeUninit;
use std::slice;

use crate::Move;

const MAX_LEGAL_MOVES: usize = 256;

pub struct MoveList {
    moves: [MaybeUninit<Move>; MAX_LEGAL_MOVES],
    num_moves: usize,
}

impl std::ops::Index<usize> for MoveList {
    type Output = Move;

    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(index < self.num_moves, "MoveList index out of bounds");

        unsafe { self.moves[index].assume_init_ref() }
    }
}

impl std::ops::IndexMut<usize> for MoveList {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        debug_assert!(index < self.num_moves, "MoveList index out of bounds");

        unsafe { self.moves[index].assume_init_mut() }
    }
}

impl MoveList {
    #[inline]
    pub fn new() -> MoveList {
        MoveList {
            moves: [MaybeUninit::uninit(); MAX_LEGAL_MOVES],
            num_moves: 0,
        }
    }

    #[inline]
    pub(super) fn add_move(&mut self, move_: Move) {
        debug_assert!(self.num_moves < MAX_LEGAL_MOVES);

        self.moves[self.num_moves].write(move_);

        self.num_moves += 1;
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.num_moves
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.num_moves == 0
    }

    #[inline]
    pub fn swap(&mut self, index1: usize, index2: usize) {
        self.moves.swap(index1, index2);
    }

    #[inline]
    pub fn get_num_moves(&self) -> usize {
        self.num_moves
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, Move> {
        unsafe {
            let ptr = self.moves.as_ptr() as *const Move;
            slice::from_raw_parts(ptr, self.num_moves)
        }
        .iter()
    }

    #[inline]
    pub fn for_each(&self, mut f: impl FnMut(&Move)) {
        for move_ in self.iter() {
            f(move_);
        }
    }
}
