use chess::Move;

use chess::board::MAX_MOVES;

use crate::Eval;

#[derive(Debug, Default, Copy, Clone)]
pub(crate) struct SearchStackEntry {
    pv_move: Move,
    ply: u8, // MAX_DEPTH = 256
    curr_move: Move,
    excl_move: Move,
    static_eval: Eval,
    move_count: u8,
    in_check: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct SearchStack {
    stack: [SearchStackEntry; MAX_MOVES],
    top: usize,
}

impl Default for SearchStack {
    fn default() -> Self {
        Self {
            stack: [SearchStackEntry::default(); MAX_MOVES],
            top: 0,
        }
    }
}

impl SearchStack {
    pub fn push(&mut self, entry: SearchStackEntry) {
        self.stack[self.top] = entry;
        self.top += 1;
    }

    pub fn pop(&mut self) -> SearchStackEntry {
        if self.is_empty() {
            panic!("Search stack is empty");
        }

        self.top -= 1;
        self.stack[self.top]
    }

    pub fn is_empty(&self) -> bool {
        self.top == 0
    }

    pub fn is_full(&self) -> bool {
        self.top == MAX_MOVES
    }

    pub fn top(&self) -> &SearchStackEntry {
        if self.is_empty() {
            panic!("Search stack is empty");
        }

        &self.stack[self.top - 1]
    }

    pub fn clear(&mut self) {
        self.top = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import SearchStack, SearchStackEntry, MAX_DEPTH
    use crate::Eval;
    use chess::Move; // Import Move

    // Helper to create a distinct SearchStackEntry for testing comparison.
    // We can vary the 'ply' to make entries distinguishable.
    fn create_test_entry(ply: u8) -> SearchStackEntry {
        // Use Move::null() as a placeholder, specific moves aren't crucial for stack logic.
        SearchStackEntry {
            ply,
            static_eval: Eval((ply as i16) * 10), // Make static_eval distinct too
            move_count: ply,
            in_check: ply % 2 == 0, // Vary in_check based on ply
            ..Default::default()    // Use default for other fields if needed
        }
    }

    #[test]
    fn test_default_state() {
        let stack = SearchStack::default();
        assert_eq!(stack.top, 0, "Initial top should be 0");
        assert!(stack.is_empty(), "New stack should be empty");
        // Assuming MAX_DEPTH is reasonably > 0
        if MAX_MOVES > 0 {
            assert!(!stack.is_full(), "New stack should not be full");
        } else {
            assert!(stack.is_full(), "Stack with MAX_DEPTH 0 should be full");
        }
    }

    #[test]
    fn test_push_single() {
        let mut stack = SearchStack::default();
        let entry = create_test_entry(1);

        stack.push(entry);

        assert_eq!(stack.top, 1, "Top should be 1 after one push");
        assert!(!stack.is_empty(), "Stack should not be empty after push");
        // Assuming MAX_DEPTH > 1
        if MAX_MOVES > 1 {
            assert!(
                !stack.is_full(),
                "Stack should not be full after one push (if MAX_DEPTH > 1)"
            );
        }
    }

    #[test]
    fn test_push_pop_single() {
        let mut stack = SearchStack::default();
        let entry_in = create_test_entry(5);

        stack.push(entry_in);
        assert_eq!(stack.top, 1, "Top should be 1 before pop");

        let entry_out = stack.pop();

        assert_eq!(stack.top, 0, "Top should be 0 after pop");
        assert!(
            stack.is_empty(),
            "Stack should be empty after popping last element"
        );

        // Verify the popped element is the one we pushed.
        // Comparing specific fields as SearchStackEntry doesn't derive PartialEq.
        assert_eq!(entry_out.ply, entry_in.ply, "Popped entry ply mismatch");
        assert_eq!(
            entry_out.static_eval, entry_in.static_eval,
            "Popped entry static_eval mismatch"
        );
        assert_eq!(
            entry_out.move_count, entry_in.move_count,
            "Popped entry move_count mismatch"
        );
        assert_eq!(
            entry_out.in_check, entry_in.in_check,
            "Popped entry in_check mismatch"
        );
        // We could compare moves too if needed, but null() == null()
        assert_eq!(entry_out.pv_move, entry_in.pv_move);
        assert_eq!(entry_out.curr_move, entry_in.curr_move);
        assert_eq!(entry_out.excl_move, entry_in.excl_move);
    }

    #[test]
    fn test_push_pop_multiple_lifo() {
        let mut stack = SearchStack::default();
        let entry1 = create_test_entry(1);
        let entry2 = create_test_entry(2);
        let entry3 = create_test_entry(3);

        stack.push(entry1);
        stack.push(entry2);
        stack.push(entry3);

        assert_eq!(stack.top, 3, "Top should be 3 after pushes");

        // Pop and check LIFO (Last In, First Out)
        let popped3 = stack.pop();
        assert_eq!(stack.top, 2, "Top should be 2 after first pop");
        assert_eq!(popped3.ply, entry3.ply, "Popped entry 3 ply mismatch");

        let popped2 = stack.pop();
        assert_eq!(stack.top, 1, "Top should be 1 after second pop");
        assert_eq!(popped2.ply, entry2.ply, "Popped entry 2 ply mismatch");

        let popped1 = stack.pop();
        assert_eq!(stack.top, 0, "Top should be 0 after third pop");
        assert!(
            stack.is_empty(),
            "Stack should be empty after popping all elements"
        );
        assert_eq!(popped1.ply, entry1.ply, "Popped entry 1 ply mismatch");
    }

    #[test]
    fn test_top_method() {
        let mut stack = SearchStack::default();
        let entry1 = create_test_entry(10);
        let entry2 = create_test_entry(20);

        stack.push(entry1);
        // Get reference to top element
        let top_ref1 = stack.top();
        assert_eq!(top_ref1.ply, entry1.ply, "Top ref 1 should point to entry1");
        assert_eq!(stack.top, 1, "Calling top() should not change stack size");

        stack.push(entry2);
        // Get reference to new top element
        let top_ref2 = stack.top();
        assert_eq!(top_ref2.ply, entry2.ply, "Top ref 2 should point to entry2");
        assert_eq!(
            stack.top, 2,
            "Calling top() again should not change stack size"
        );

        // Pop and check top again
        let _popped = stack.pop();
        let top_ref_after_pop = stack.top();
        assert_eq!(
            top_ref_after_pop.ply, entry1.ply,
            "Top ref after pop should point back to entry1"
        );
        assert_eq!(stack.top, 1, "Stack size should be 1 after pop");
    }

    #[test]
    #[should_panic(expected = "Search stack is empty")] // Or index out of bounds depending on exact panic
    fn test_pop_empty_panic() {
        let mut stack = SearchStack::default();
        assert!(stack.is_empty());
        stack.pop(); // Should panic
    }

    #[test]
    #[should_panic(expected = "Search stack is empty")] // Or index out of bounds depending on exact panic
    fn test_top_empty_panic() {
        let stack = SearchStack::default();
        assert!(stack.is_empty());
        stack.top(); // Should panic
    }

    #[test]
    fn test_clear() {
        let mut stack = SearchStack::default();
        stack.push(create_test_entry(1));
        stack.push(create_test_entry(2));
        assert_eq!(stack.top, 2);
        assert!(!stack.is_empty());

        stack.clear();

        assert_eq!(stack.top, 0, "Top should be 0 after clear");
        assert!(stack.is_empty(), "Stack should be empty after clear");
        if MAX_MOVES > 0 {
            assert!(!stack.is_full(), "Stack should not be full after clear");
        }

        // Test clearing an already empty stack
        stack.clear();
        assert_eq!(
            stack.top, 0,
            "Top should remain 0 after clearing empty stack"
        );
        assert!(
            stack.is_empty(),
            "Stack should remain empty after clearing empty stack"
        );
    }

    #[test]
    fn test_is_full() {
        // Only run this test if MAX_DEPTH is reasonably small to avoid long test times
        // MAX_DEPTH = 256 is acceptable. If it were huge, we might skip or mock.
        if MAX_MOVES == 0 {
            let stack = SearchStack::default();
            assert!(stack.is_full(), "Stack with MAX_DEPTH 0 is always full");
            return;
        }

        let mut stack = SearchStack::default();
        // Fill the stack exactly to MAX_DEPTH
        for i in 0..MAX_MOVES {
            assert!(
                !stack.is_full(),
                "Stack should not be full before push #{}",
                i + 1
            );
            assert_eq!(
                stack.top,
                i,
                "Stack top should be {} before push #{}",
                i,
                i + 1
            );
            stack.push(create_test_entry(i as u8));
        }

        assert_eq!(
            stack.top, MAX_MOVES,
            "Stack top should be MAX_DEPTH when full"
        );
        assert!(
            stack.is_full(),
            "Stack should be full after MAX_DEPTH pushes"
        );

        // Pop one element
        stack.pop();
        assert_eq!(
            stack.top,
            MAX_MOVES - 1,
            "Stack top should decrease after pop"
        );
        assert!(
            !stack.is_full(),
            "Stack should not be full after popping one element"
        );
    }

    #[test]
    #[should_panic(expected = "index out of bounds")] // Pushing past MAX_DEPTH causes array index panic
    fn test_push_full_panic() {
        // Only makes sense if MAX_DEPTH > 0
        if MAX_MOVES == 0 {
            // We can't push onto a 0-sized stack, so the panic condition is different
            // or maybe impossible depending on how 0-sized arrays behave.
            // Let's just skip this specific panic test for MAX_DEPTH == 0.
            // Alternatively, create a specific test for MAX_DEPTH == 0 push panic.
            return;
        }

        let mut stack = SearchStack::default();
        // Fill the stack
        for i in 0..MAX_MOVES {
            stack.push(create_test_entry(i as u8));
        }
        assert!(stack.is_full());

        // This push should panic
        stack.push(create_test_entry(MAX_MOVES as u8));
    }
}
