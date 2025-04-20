//! # Module: `board::history`
//!
//! This module defines the `UndoHistory` struct, which is used to store a history of
//! `BoardState`s. This history is essential for implementing the undo functionality in
//! the chess engine, allowing the game to revert to previous states.
//!
//! ## Overview
//!
//! The `UndoHistory` struct is designed as a fixed-size circular buffer. It stores a
//! limited number of `BoardState`s, overwriting the oldest state when the buffer is full.
//! This approach ensures that the memory usage of the history is bounded, which is
//! important for performance and resource management.
//!
//! ## Key Components
//!
//! - **`UndoHistory<const N: usize>`**: A struct representing the undo history.
//!   - `arr`: A fixed-size array of `BoardState`s.
//!   - `count`: The number of states pushed onto the history.
//!
//! ## Functionality
//!
//! - **`push(state: BoardState)`**: Adds a new `BoardState` to the history. If the
//!   history is full, the oldest state is overwritten.
//! - **`nth(index: usize)`**: Retrieves the `BoardState` at a specific index in the
//!   history.
//! - **`last()`**: Retrieves the most recently pushed `BoardState`.
//! - **`len()`**: Returns the number of states in the history.
//! - **`is_empty()`**: Checks if the history is empty.
//! - **`pop_unchecked()`**: Removes and returns the most recently pushed `BoardState`
//!   without checking if the history is empty. This method is unsafe if called on an
//!   empty history.
//! - **`pop()`**: Removes and returns the most recently pushed `BoardState`, or `None`
//!   if the history is empty.
//! - **`clear()`**: Clears the history, resetting the count to zero.
//!
//! ## Usage
//!
//! The `UndoHistory` is used by the `Board` struct to manage the game's history. The
//! `Board::store_state()` method pushes the current `BoardState` onto the history, and
//! `Board::restore_state()` retrieves the last state from the history, effectively
//! undoing the last

use super::{Board, BoardState};

/// A fixed-size undo history buffer using a circular array.
///
/// Stores the last `N` `BoardState`s pushed onto it. When the capacity `N` is
/// reached, adding a new state overwrites the oldest state.
///
/// The `count` field tracks the total number of states pushed since the last clear,
/// which determines the logical index for accessing states within the circular buffer.
#[derive(Debug)]
pub struct UndoHistory<const N: usize> {
    /// The circular array storing the board states.
    arr: [BoardState; N],
    /// The total number of states pushed since the last clear.
    /// Used to calculate the insertion/retrieval index modulo `N`.
    count: usize,
}

impl<const N: usize> Default for UndoHistory<N> {
    /// Creates a new, empty `UndoHistory` with capacity `N`.
    ///
    /// The internal array is initialized with default `BoardState` values.
    fn default() -> Self {
        // Note: This requires BoardState to implement Default and Copy,
        // or for `[BoardState::default(); N]` syntax to be valid.
        // Assuming BoardState implements Default.
        Self {
            arr: [BoardState::default(); N],
            count: 0,
        }
    }
}

impl<const N: usize> UndoHistory<N> {
    /// Pushes a new board state onto the history.
    ///
    /// If the history has reached its capacity `N`, the oldest state
    /// is overwritten.
    ///
    /// # Arguments
    ///
    /// * `state` - The `BoardState` to add to the history.
    pub fn push(&mut self, state: BoardState) {
        // The modulo operator ensures we wrap around the array,
        // effectively making it circular.
        self.arr[self.count % N] = state;
        self.count += 1;
    }

    /// Returns a reference to the state at the given logical index.
    ///
    /// The index refers to the sequence of pushes since the last clear.
    /// `nth(0)` would be the first state pushed, `nth(len() - 1)` the last.
    ///
    /// Returns `None` if the index is out of bounds *of the underlying array*,
    /// although with the current implementation using `index % N`, this primarily
    /// depends on whether `N > 0`.
    ///
    /// **Note:** This method currently retrieves the state at the physical
    /// position `index % N` in the array. If `count > N`, indices less than
    /// `count - N` will refer to states that have been logically overwritten,
    /// returning the newer state occupying that physical slot. A more robust
    /// implementation might return `None` for overwritten states.
    ///
    /// # Arguments
    ///
    /// * `index` - The logical index of the state to retrieve.
    pub fn nth(&self, index: usize) -> Option<&BoardState> {
        // `get` provides bounds checking, but index % N is always < N (for N > 0).
        // The logical validity (index < self.count and not overwritten) isn't checked here.
        self.arr.get(index % N)
    }

    /// Returns a reference to the most recently pushed state, if any.
    ///
    /// Returns `None` if the history is empty.
    ///
    /// **Note:** Relies on the behavior of `nth`. If the history is empty (`count == 0`),
    /// the current implementation might lead to unexpected behavior due to usize underflow
    /// when calculating `self.count - 1`.
    pub fn last(&self) -> Option<&BoardState> {
        if self.is_empty() {
            None
        } else {
            // Calculate the index of the last element correctly using modulo arithmetic.
            // (self.count - 1) % N gives the correct physical index.
            self.nth(self.count - 1)
        }
    }

    /// Returns the total number of states pushed since the last clear.
    ///
    /// Note that this can be greater than the capacity `N` if states
    /// have been overwritten.
    pub fn len(&self) -> usize {
        self.count
    }

    /// Returns `true` if no states have been pushed since the last clear.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Removes and returns the most recently pushed state without checking if the history is empty.
    ///
    /// # Panics / Undefined Behavior
    ///
    /// Calling this method on an empty history will lead to `usize` underflow
    /// when decrementing `self.count`, resulting in accessing an incorrect index
    /// (`usize::MAX % N`) and returning potentially invalid data.
    ///
    /// **Use `pop()` for a safe alternative.**
    ///
    /// # Returns
    ///
    /// The last pushed `BoardState`.
    pub fn pop_unchecked(&mut self) -> BoardState {
        // Decrement count *before* accessing the array to get the last element's index.
        self.count -= 1;
        // Access the element at the now-correct index for the last state.
        // Assumes BoardState is Copy. If not, might need clone or read/write logic.
        // If BoardState is not Copy, this might return a default/uninitialized value
        // if the array was initialized with defaults and not overwritten yet.
        // Assuming BoardState is Copy for simplicity based on typical usage.
        self.arr[self.count % N]
    }

    /// Removes and returns the most recently pushed state, if any.
    ///
    /// Returns `None` if the history is empty.
    pub fn pop(&mut self) -> Option<BoardState> {
        if self.is_empty() {
            None
        } else {
            // Safe wrapper around pop_unchecked
            Some(self.pop_unchecked())
        }
    }

    /// Clears the history.
    ///
    /// Resets the count to zero, effectively making the history empty.
    /// The underlying array elements are not modified but will be overwritten
    /// by subsequent `push` operations.
    pub fn clear(&mut self) {
        self.count = 0;
    }
}

// --- Documentation for Board methods remains the same ---

impl Board {
    /// # Store State
    /// - Stores the current board state in the history stack
    ///
    /// ## Example
    ///
    /// ```
    /// use sophos::core::*;
    /// let mut board = Board::default();
    /// board.store_state();
    /// assert_eq!(board.history.len(), 1);
    /// ```
    ///
    pub(super) fn store_state(&mut self) {
        self.history.push(self.state);
    }

    /// # Restore State
    ///
    /// - Restores the board to the previous state
    /// - Pops the last board state from the history stack
    /// - Updates the board state
    ///
    /// ## Panics
    ///
    /// - Panics if the history stack is empty (in debug builds due to `debug_assert!`).
    ///   In release builds, calling this on an empty history invokes the
    ///   potentially problematic behavior of `pop_unchecked`.
    ///
    /// ## Arguments
    ///
    /// * `self` - The board to restore the state of
    pub(super) fn restore_state(&mut self) {
        // The debug_assert provides safety during development.
        debug_assert!(!self.history.is_empty(), "History stack is empty!");
        // Relies on the caller ensuring history is not empty, or accepts
        // the consequences of pop_unchecked on an empty history in release builds.
        self.state = self.history.pop_unchecked();
    }
}
