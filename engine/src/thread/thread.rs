use std::{
    collections::HashMap,
    iter,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
};

use chess::{Move, board::Board};

use crate::{
    TimeControl,
    search::TT,
    search::{Clock, SearchWorker},
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

        let current_size = self.workers.len() + 1;
        if new_size > current_size {
            for i in current_size..new_size {
                let worker = SearchWorker::new(i, self.stop.clone(), self.nodes.clone());
                self.workers.push(worker);
            }
        } else if new_size < current_size {
            self.workers.truncate(new_size - 1);
        }

        self.reset();
    }

    pub fn reset(&mut self) {
        self.main_worker.reset();
        for worker in &mut self.workers {
            worker.reset();
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

            main_worker.prepare_search();
            s.spawn(move || {
                main_worker.setup(board_clone);
                main_worker.iterative_deepening(tt);
            });

            for worker in workers {
                worker.prepare_search();
                let board_clone = board.clone();
                s.spawn(move || {
                    worker.setup(board_clone);
                    worker.iterative_deepening(tt);
                });
            }
        });

        self.stop.store(true, Ordering::Relaxed);

        println!("bestmove {}", self.find_best_move().to_str(board));
    }

    fn find_best_move(&self) -> Move {
        let threads = iter::once(&self.main_worker).chain(self.workers.iter());

        let highest_depth = threads
            .clone()
            .max_by(|a, b| a.depth.cmp(&b.depth))
            .unwrap()
            .depth;

        threads
            .filter(|w| w.depth == highest_depth)
            .map(|w| w.best_move())
            .fold(HashMap::new(), |mut acc, move_| {
                *acc.entry(move_).or_insert(0) += 1;
                acc
            })
            .into_iter()
            .max_by_key(|(_, count)| *count)
            .map(|(move_, _)| move_)
            .unwrap_or(Move::NULL)
    }

    pub fn nodes(&self) -> u64 {
        self.nodes.load(Ordering::Relaxed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::TT;
    use std::sync::atomic::Ordering;

    fn create_test_tt() -> TT {
        let mut tt = TT::new();
        tt.resize(1);
        tt
    }

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
    }

    #[test]
    fn test_resize_increase() {
        let stop = create_test_stop();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        assert_eq!(pool.size(), 1);

        pool.resize(4);
        assert_eq!(pool.size(), 4, "Pool size should be 4 after resize");
        assert_eq!(pool.workers.len(), 3, "Should have 3 workers after resize");

        assert_eq!(pool.workers[0].thread_id(), 1, "Worker 0 should have ID 1");
        assert_eq!(pool.workers[1].thread_id(), 2, "Worker 1 should have ID 2");
        assert_eq!(pool.workers[2].thread_id(), 3, "Worker 2 should have ID 3");
    }

    #[test]
    fn test_resize_decrease() {
        let stop = create_test_stop();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(5);
        assert_eq!(pool.size(), 5);
        assert_eq!(pool.workers.len(), 4);

        pool.resize(2);
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

        pool.resize(3);
        assert_eq!(pool.size(), 3, "Pool size should remain 3");
        assert_eq!(pool.workers.len(), 2, "Worker count should remain 2");
    }

    #[test]
    fn test_resize_to_one() {
        let stop = create_test_stop();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(4);
        assert_eq!(pool.size(), 4);

        pool.resize(1);
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

        pool.resize(0);
        assert_eq!(pool.size(), 1, "Pool size should be 1 after resize to 0");
        assert!(
            pool.workers.is_empty(),
            "Workers vec should be empty after resize to 0"
        );
    }

    #[test]
    fn test_start_search_runs_and_stops() {
        let stop = create_test_stop();
        let tt = create_test_tt();
        let board = Board::default();
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(3);

        assert!(!stop.load(Ordering::Relaxed));

        pool.start_search(TimeControl::FixedTime(300), &tt, &board);
    }
}
