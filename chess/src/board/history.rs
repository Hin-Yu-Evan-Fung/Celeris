use super::{Board, BoardState};
use std::mem::MaybeUninit; // Import MaybeUninit

/// A fixed-size undo history buffer using a circular array.
///
/// Stores the last `N` `BoardState`s pushed onto it. When the capacity `N` is
/// reached, adding a new state overwrites the oldest state.
///
/// The `count` field tracks the total number of states pushed since the last clear,
/// which determines the logical index for accessing states within the circular buffer.
/// Uses `MaybeUninit` to avoid initializing the array elements upfront.
#[derive(Debug)]
pub struct UndoHistory<const N: usize> {
    /// The circular array storing the board states, potentially uninitialized.
    /// Safety Invariant: An element at physical index `i` is considered initialized
    /// if `count > 0` and the logical index `idx` corresponding to `i`
    /// (where `idx % N == i`) satisfies `count - N <= idx < count`.
    arr: [MaybeUninit<BoardState>; N],
    /// The total number of states pushed since the last clear.
    /// Used to calculate the insertion/retrieval index modulo `N`.
    count: usize,
}

// Cannot derive Default easily with MaybeUninit array without unsafe or nightly features.
// Implement it manually.
impl<const N: usize> Default for UndoHistory<N> {
    /// Creates a new, empty `UndoHistory` with capacity `N`.
    ///
    /// The internal array is allocated but not initialized.
    fn default() -> Self {
        // Safety: Creating an array of `MaybeUninit` is safe.
        // `MaybeUninit::uninit()` creates a single uninitialized value.
        // The `[...; N]` syntax relies on `MaybeUninit<T>` being `Copy`, which it is.
        // This allocates the space for `N` `BoardState`s without running `BoardState::default()`.
        Self {
            arr: [MaybeUninit::uninit(); N],
            count: 0,
        }
    }
}

impl<const N: usize> UndoHistory<N> {
    /// Pushes a new board state onto the history.
    ///
    /// If the history has reached its capacity `N`, the oldest state
    /// is overwritten. This operation is SAFE because `MaybeUninit::write`
    /// does not read the old value.
    ///
    /// # Arguments
    ///
    /// * `state` - The `BoardState` to add to the history.
    #[inline]
    pub fn push(&mut self, state: BoardState) {
        // Write the state into the MaybeUninit wrapper. This marks the slot as initialized.
        // If the slot was already initialized (in case of overwrite), `write` handles it correctly
        // for Copy types by simply overwriting. If BoardState needed dropping, we'd need
        // unsafe { self.arr[index].assume_init_drop() } here first.
        self.arr[self.count].write(state);
        self.count += 1;
    }

    /// Returns an iterator for the undo state list
    ///
    /// ## Arguments
    ///
    /// - `roll_back` - the number of elements in the list (roll back to a certain history)
    ///
    /// ## Returns
    ///
    /// - iterator - an iterator that starts from the roll back history all the way up to the present
    #[inline]
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &BoardState> {
        (0..self.count).map(|i| unsafe { self.arr[i].assume_init_ref() })
    }

    /// Returns a reference to the state at the given logical index.
    ///
    /// The index refers to the sequence of pushes since the last clear.
    /// `nth(0)` would be the first state pushed, `nth(len() - 1)` the last.
    ///
    /// Returns `None` if the logical index is out of bounds (`>= count`) or
    /// refers to a state that has been overwritten by the circular buffer logic.
    ///
    /// # Arguments
    ///
    /// * `index` - The logical index of the state to retrieve (0 <= index < count).
    #[inline]
    pub fn nth(&self, index: usize) -> Option<&BoardState> {
        // Check if the logical index is valid and refers to a non-overwritten state.
        // A state is valid if its logical index `index` is within the range
        // [count - capacity, count - 1], where capacity is min(count, N).
        // This simplifies to: index < count && index >= count.saturating_sub(N)
        if N > 0 && index < self.count && index >= self.count.saturating_sub(N) {
            // Safety:
            // 1. We've checked that the logical `index` corresponds to a state that
            //    was pushed and has not been overwritten.
            // 2. Therefore, the element at `physical_index` is guaranteed to be initialized.
            // 3. `assume_init_ref()` provides a reference to the initialized `BoardState`.
            //    The lifetime of the reference is tied to `&self`.
            unsafe { Some(self.arr[index].assume_init_ref()) }
        } else {
            None // Index out of bounds or points to an overwritten slot.
        }
    }

    /// Returns a reference to the most recently pushed state, if any.
    ///
    /// Returns `None` if the history is empty.
    #[inline]
    pub fn last(&self) -> Option<&BoardState> {
        if self.is_empty() {
            None
        } else {
            // The last logical index is count - 1.
            self.nth(self.count - 1)
        }
    }

    /// Returns the total number of states pushed since the last clear.
    ///
    /// Note that this can be greater than the capacity `N` if states
    /// have been overwritten.
    #[inline]
    pub fn len(&self) -> usize {
        self.count
    }

    /// Returns `true` if no states have been pushed since the last clear.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Removes and returns the most recently pushed state, if any.
    ///
    /// Returns `None` if the history is empty.
    #[inline]
    pub fn pop(&mut self) -> Option<BoardState> {
        if self.is_empty() {
            None
        } else {
            // Safe wrapper around pop_unchecked
            Some(self.pop_unchecked())
        }
    }

    /// Removes and returns the most recently pushed state without checking if the history is empty.
    ///
    /// # Safety / Panics
    ///
    /// Calling this method on an empty history (`count == 0`) will cause `usize` underflow
    /// when decrementing `self.count`, leading to a panic in debug builds or incorrect
    /// memory access (reading uninitialized or unrelated data) in release builds.
    ///
    /// **Use `pop()` for a safe alternative.**
    ///
    /// # Returns
    ///
    /// The last pushed `BoardState`. Assumes `BoardState` is `Copy`.
    #[inline]
    pub fn pop_unchecked(&mut self) -> BoardState {
        // This debug_assert helps catch misuse during development.
        debug_assert!(!self.is_empty(), "pop_unchecked called on empty history");

        // Decrement count *before* calculating the index.
        self.count -= 1;

        // Safety:
        // 1. The caller guarantees `count` was > 0 before the call.
        // 2. `self.count` now holds the logical index of the element we want to read.
        // 3. This logical index corresponds to the last pushed, non-overwritten state.
        // 4. Therefore, `self.arr[index]` is guaranteed to be initialized.
        // 5. `assume_init_read()` safely reads the value (assuming BoardState is Copy).
        unsafe { self.arr[self.count].assume_init_read() }
    }

    // pop_mut_unchecked needs correction too
    #[inline]
    pub fn pop_mut_unchecked(&mut self) -> &mut BoardState {
        debug_assert!(
            !self.is_empty(),
            "pop_mut_unchecked called on empty history"
        );
        self.count -= 1;
        let index = self.count % N; // Correct index for circular buffer
        // Safety: assume_init_mut is safe as we know it's initialized. Returns mutable ref.
        unsafe { self.arr[index].assume_init_mut() }
    }

    /// Clears the history.
    ///
    /// Resets the count to zero, effectively making the history empty.
    /// The underlying array elements are not modified (remain potentially initialized
    /// or uninitialized) but will be overwritten by subsequent `push` operations.
    /// The `MaybeUninit` wrappers don't need explicit dropping.
    #[inline]
    pub fn clear(&mut self) {
        // Note: If BoardState implemented Drop, we would need to iterate through
        // the initialized elements (from count.saturating_sub(N) up to count-1)
        // and call `assume_init_drop()` on them here before resetting count.
        // Since we assume BoardState is Copy, this is not needed.
        self.count = 0;
    }
}

// --- Board methods using history remain the same ---

impl Board {
    /// Stores the current board state in the history stack.
    #[inline]
    pub(super) fn store_state(&mut self) {
        self.history.push(self.state);
    }

    /// Restores the board to the previous state by popping from history.
    ///
    /// # Panics
    ///
    /// - Panics if the history stack is empty (due to `pop_unchecked`).
    ///   Use only after ensuring a state can be restored.
    #[inline]
    pub(super) fn restore_state(&mut self) {
        // The safety burden is on the caller (e.g., make_move/unmake_move logic)
        // to ensure history is not empty before calling restore_state.
        // Adding a debug_assert here is still good practice.
        debug_assert!(
            !self.history.is_empty(),
            "Attempted to restore state from empty history!"
        );
        // self.state = self.history.pop_unchecked();

        std::mem::swap(&mut self.state, self.history.pop_mut_unchecked());
    }
}
