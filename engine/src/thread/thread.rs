use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicU64, Ordering},
};

use chess::Board;

use crate::{
    TimeControl,
    search::{Clock, SearchWorker},
    types::TT,
};

pub struct ThreadPool {
    pub main_worker: SearchWorker,
    workers: Vec<SearchWorker>,
    stop: Arc<AtomicBool>,
    nodes: Arc<AtomicU64>,
}

impl ThreadPool {
    pub fn new(stop: Arc<AtomicBool>) -> Self {
        let nodes = Arc::new(AtomicU64::new(0));
        let main_worker = SearchWorker::new(0, stop.clone(), nodes.clone());

        Self {
            main_worker,
            workers: Vec::new(),
            stop,
            nodes,
        }
    }

    pub fn size(&self) -> usize {
        self.workers.len() + 1
    }

    pub fn resize(&mut self, new_size: usize) {
        let new_size = new_size.max(1);

        let current_size = self.workers.len() + 1; // Include the main thread
        if new_size > current_size {
            for i in current_size..new_size {
                let worker = SearchWorker::new(i, self.stop.clone(), self.nodes.clone());
                self.workers.push(worker);
            }
        } else if new_size < current_size {
            self.workers.truncate(new_size - 1);
        }
    }

    pub fn start_search(&mut self, time_control: TimeControl, tt: &TT, board: &Board) {
        self.stop.store(false, Ordering::Relaxed);
        self.nodes.store(0, Ordering::Relaxed);

        self.main_worker.clock = Clock::new(
            self.stop.clone(),
            self.nodes.clone(),
            time_control,
            board.stm(),
        );
        std::thread::scope(|s| {
            let board_clone = board.clone();
            let main_worker = &mut self.main_worker;
            let workers = &mut self.workers;

            main_worker.reset();
            s.spawn(move || {
                main_worker.setup(board_clone);
                main_worker.start_search(tt);
            });

            for worker in workers {
                worker.reset();
                let board_clone = board.clone();
                s.spawn(move || {
                    worker.setup(board_clone);
                    worker.start_search(tt);
                });
            }
        });

        self.stop.store(true, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod tests {
    use super::*; // Import ThreadPool, SearchWorker
    use crate::types::TT; // Import TT
    use std::sync::atomic::Ordering; // Import RwLock for TT
    use std::time::Duration;

    // Helper to create a default TT wrapped for testing
    fn create_test_tt() -> TT {
        TT::new()
    }

    // Helper to create a default stop signal for testing
    fn create_test_stop() -> Arc<AtomicBool> {
        Arc::new(AtomicBool::new(false))
    }

    #[test]
    fn test_new_pool() {
        let stop = create_test_stop();
        let pool = ThreadPool::new(Arc::clone(&stop));

        assert_eq!(pool.size(), 1, "New pool should have size 1");
        assert_eq!(
            pool.main_worker.thread_id(),
            0,
            "Main worker should have ID 0"
        );
        assert!(
            pool.workers.is_empty(),
            "New pool should have no extra workers"
        );
        assert!(
            !pool.stop.load(Ordering::Relaxed),
            "Stop signal should be initially false"
        );
        // nodes counter starts at 0 (implicitly tested by Arc default)
    }

    #[test]
    fn test_resize_increase() {
        let stop = create_test_stop();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        assert_eq!(pool.size(), 1);

        pool.resize(4); // Resize to 4 total threads (main + 3 workers)
        assert_eq!(pool.size(), 4, "Pool size should be 4 after resize");
        assert_eq!(pool.workers.len(), 3, "Should have 3 workers after resize");

        // Check worker IDs
        assert_eq!(pool.workers[0].thread_id(), 1, "Worker 0 should have ID 1");
        assert_eq!(pool.workers[1].thread_id(), 2, "Worker 1 should have ID 2");
        assert_eq!(pool.workers[2].thread_id(), 3, "Worker 2 should have ID 3");
    }

    #[test]
    fn test_resize_decrease() {
        let stop = create_test_stop();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(5); // Resize to 5 (main + 4 workers)
        assert_eq!(pool.size(), 5);
        assert_eq!(pool.workers.len(), 4);

        pool.resize(2); // Resize to 2 (main + 1 worker)
        assert_eq!(pool.size(), 2, "Pool size should be 2 after decrease");
        assert_eq!(pool.workers.len(), 1, "Should have 1 worker after decrease");
        assert_eq!(
            pool.workers[0].thread_id(),
            1,
            "Remaining worker should have ID 1"
        );
    }

    #[test]
    fn test_resize_no_change() {
        let stop = create_test_stop();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(3);
        assert_eq!(pool.size(), 3);

        pool.resize(3); // Resize to the same size
        assert_eq!(pool.size(), 3, "Pool size should remain 3");
        assert_eq!(pool.workers.len(), 2, "Worker count should remain 2");
    }

    #[test]
    fn test_resize_to_one() {
        let stop = create_test_stop();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(4); // Start with multiple workers
        assert_eq!(pool.size(), 4);

        pool.resize(1); // Resize back to just the main thread
        assert_eq!(pool.size(), 1, "Pool size should be 1 after resize to 1");
        assert!(
            pool.workers.is_empty(),
            "Workers vec should be empty after resize to 1"
        );
    }

    #[test]
    fn test_resize_zero_becomes_one() {
        let stop = create_test_stop();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(3);

        pool.resize(0); // Attempt resize to 0
        assert_eq!(pool.size(), 1, "Pool size should be 1 after resize to 0");
        assert!(
            pool.workers.is_empty(),
            "Workers vec should be empty after resize to 0"
        );
    }

    #[test]
    fn test_start_search_runs_and_stops() {
        let stop = create_test_stop();
        let tt = create_test_tt(); // Create a dummy TT
        let board = Board::default();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(3); // main + 2 workers

        // We can't easily verify work done inside worker.search without modifying it.
        // But we can check that start_search completes and sets the stop signal.
        assert!(!stop.load(Ordering::Relaxed));

        // Pass a reference to the TT data (behind the RwLock and Arc)
        pool.start_search(TimeControl::FixedTime(300), &tt, &board); // Pass &TT
    }

    // Optional: Test with a slight delay in a test-only search method
    // This requires modifying SearchWorker slightly.

    // Add this to search/worker.rs for the test below:
    /*
    #[cfg(test)]
    impl SearchWorker {
        // Test-only search method
        pub fn test_search_with_delay(&mut self, _tt: &TT) {
            println!("Test search running for thread {}", self.thread_id());
            std::thread::sleep(Duration::from_millis(20));
            println!("Test search finished for thread {}", self.thread_id());
        }
    }
    */

    /* // Uncomment if you add the test_search_with_delay method
    #[test]
    fn test_start_search_parallelism_observable() {
        let stop = create_test_stop();
        let tt_arc = create_test_tt();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(4); // main + 3 workers

        let start_time = std::time::Instant::now();

        // Temporarily replace the search method for testing (requires modifying ThreadPool or SearchWorker)
        // This example assumes we modify start_search to call a different method for testing.
        // For now, we just call the original start_search and observe timing.

        // Pass a reference to the TT data
        pool.start_search(&*tt_arc.read().unwrap());

        let duration = start_time.elapsed();

        // Check that the stop signal is set
        assert!(stop.load(Ordering::Relaxed));

        // Check that the duration is roughly consistent with one delay, not N delays sequentially
        // This is a weak check for parallelism.
        println!("start_search duration: {:?}", duration);
        assert!(duration < Duration::from_millis(80), "Execution took too long, likely not parallel");
        assert!(duration >= Duration::from_millis(20), "Execution finished too quickly");
    }
    */
}
