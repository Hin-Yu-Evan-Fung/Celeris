use crate::{Move, board::Board};

const MAX_LEGAL_MOVES: usize = 218;

pub struct MoveList {
    moves: [Move; MAX_LEGAL_MOVES],
    num_moves: usize,
}

impl MoveList {
    #[inline]
    pub fn new() -> MoveList {
        MoveList {
            moves: [Move::NULL; MAX_LEGAL_MOVES],
            num_moves: 0,
        }
    }

    #[inline]
    pub(super) fn add_move(&mut self, move_: Move) {
        // Safety - since no one can access the add_move function, and the maximum number of legal moves stored in this list is MAX_LEGAL_MOVES, num_moves will never exceed the boundary
        debug_assert!(self.num_moves < MAX_LEGAL_MOVES);
        unsafe {
            *self.moves.get_unchecked_mut(self.num_moves) = move_;
        }
        self.num_moves += 1;
    }

    #[inline]
    pub(super) fn pop_move(&mut self) -> Move {
        self.num_moves -= 1;
        unsafe { *self.moves.get_unchecked(self.num_moves) }
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
    pub fn get_move(&self, index: usize) -> Move {
        self.moves[index]
    }

    #[inline]
    pub fn get_num_moves(&self) -> usize {
        self.num_moves
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &Move> {
        self.moves[..self.num_moves].iter()
    }

    #[inline]
    pub fn for_each(&self, mut f: impl FnMut(&Move)) {
        for i in 0..self.num_moves {
            f(&self.moves[i]);
        }
    }
}
