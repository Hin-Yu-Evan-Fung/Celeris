use std::{
    env::current_exe,
    path::MAIN_SEPARATOR_STR,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
};

use crate::search::SearchWorker;

use super::{BoxedJob, WorkerThread};

pub struct ThreadPool {
    main_thread: WorkerThread,
    workers: Vec<WorkerThread>,
    stop: Arc<AtomicBool>,
    nodes: Arc<AtomicU64>,
}

impl ThreadPool {
    pub fn new(stop: Arc<AtomicBool>) -> Self {
        let nodes = Arc::new(AtomicU64::new(0));

        let main_thread = WorkerThread::new(0, Arc::clone(&stop));

        Self {
            main_thread,
            workers: Vec::new(),
            stop,
            nodes,
        }
    }

    // Dynamically resize the pool, either adding or removing workers
    pub fn resize(&mut self, new_size: usize) {
        let new_size = new_size.max(1);

        let current_size = self.workers.len() + 1; // Include the main thread
        if new_size > current_size {
            for i in current_size..new_size {
                let worker = WorkerThread::new(i, Arc::clone(&self.stop));
                self.workers.push(worker);
            }
        } else if new_size < current_size {
            self.workers.truncate(new_size - 1);
        }
    }

    /// Executes a single job on the first available worker thread (main or worker).
    ///
    /// If all threads are currently busy, this function will block and poll
    /// until a thread becomes idle to accept the job.
    ///
    /// # Arguments
    ///
    /// * `job` - The `BoxedJob` to execute. Note this takes a single job,
    ///           not a factory, as only one thread will execute it.
    ///
    pub fn execute_on_first_available(&self, job: BoxedJob) {
        // Loop until we successfully dispatch the job
        loop {
            // Prioritize checking the main thread (arbitrary choice)
            if self.main_thread.is_idle() {
                self.main_thread.execute(job);
                return; // Job dispatched
            }

            // Check worker threads
            for worker in &self.workers {
                if worker.is_idle() {
                    worker.execute(job);
                    return; // Job dispatched
                }
            }
            // Alternatively, yield might be slightly better sometimes:
            std::thread::yield_now();
        }
    }

    pub fn execute_for_all<F>(&self, job_factory: F)
    where
        F: Fn() -> BoxedJob,
    {
        self.main_thread.execute(job_factory());

        for worker in &self.workers {
            worker.execute(job_factory());
        }
    }

    pub fn sync_execute_for_all<F>(&self, job_factory: F)
    where
        F: Fn() -> BoxedJob,
    {
        self.wait_for_all_idle();

        self.execute_for_all(job_factory);
    }

    pub fn wait_for_all_idle(&self) {
        self.main_thread.wait_for_idle();

        for worker in &self.workers {
            worker.wait_for_idle();
        }
    }

    pub fn shutdown(&mut self) {
        self.main_thread.shutdown();

        for worker in &mut self.workers {
            worker.shutdown();
        }
    }

    pub fn stop_all(&self) {
        self.stop.store(true, Ordering::Relaxed);
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        self.wait_for_all_idle();
        self.shutdown();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_single_job_execution() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut pool = ThreadPool::new(Arc::clone(&stop));

        pool.resize(10);

        let job_factory = || {
            let job: BoxedJob = Box::new(|worker, stop| {
                let mut worker = worker.lock().unwrap();
                worker.reset(); // Your search logic here

                println!("Worker Reset!");

                if stop.load(Ordering::Relaxed) {
                    println!("Stopped search early");
                }
            });

            job
        };

        pool.sync_execute_for_all(job_factory);
    }

    #[test]
    fn test_multiple_job_execution() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut pool = ThreadPool::new(Arc::clone(&stop));

        pool.resize(10);

        for _ in 0..8 {
            let job_factory = || {
                let job: BoxedJob = Box::new(|worker, stop| {
                    let mut worker = worker.lock().unwrap();
                    worker.reset(); // Your search logic here

                    println!("Worker Reset for thread {}!", worker.thread_id);

                    if stop.load(Ordering::Relaxed) {
                        println!("Stopped search early");
                    }
                });

                job
            };

            pool.sync_execute_for_all(job_factory);
        }
    }

    #[test]
    fn test_clean_shutdown() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut pool = ThreadPool::new(Arc::clone(&stop));

        pool.resize(10);

        let job_factory = || {
            let job: BoxedJob = Box::new(|worker, stop| {
                let mut worker = worker.lock().unwrap();
                worker.reset(); // Your search logic here

                println!("Worker Reset!");

                if stop.load(Ordering::Relaxed) {
                    println!("Stopped search early");
                }
            });

            job
        };

        pool.sync_execute_for_all(job_factory);

        // Should shutdown without panic
        pool.shutdown();
    }

    #[test]
    fn test_stop_signal_respected() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut pool = ThreadPool::new(Arc::clone(&stop));

        pool.resize(10);

        let job_factory = || {
            let job: BoxedJob = Box::new(|_, stop| {
                while !stop.load(Ordering::Relaxed) {
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
            });

            job
        };

        pool.sync_execute_for_all(job_factory);

        std::thread::sleep(std::time::Duration::from_millis(500));

        pool.stop_all();

        pool.wait_for_all_idle();
    }

    #[test]
    fn test_spurious_wakeup_tolerance() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut pool = ThreadPool::new(Arc::clone(&stop));

        pool.resize(10);

        let job_factory = || {
            let job: BoxedJob = Box::new(|_, _| {
                std::thread::sleep(std::time::Duration::from_millis(50));
            });

            job
        };

        pool.sync_execute_for_all(job_factory);

        // Early wait, job is not finished yet
        pool.wait_for_all_idle();

        // After waiting properly, should be idle
        assert!(pool.main_thread.is_idle());
        for worker in &pool.workers {
            assert!(worker.is_idle());
        }
    }

    #[test]
    fn test_stress_many_jobs() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut pool = ThreadPool::new(Arc::clone(&stop));

        pool.resize(10);

        let counter = Arc::new(std::sync::Mutex::new(0));

        for _ in 0..100 {
            let counter_clone = Arc::clone(&counter);
            let job_factory = move || {
                let counter_clone = Arc::clone(&counter_clone);
                let job: BoxedJob = Box::new(move |_worker, _stop| {
                    let mut count = counter_clone.lock().unwrap();
                    *count += 1;
                });

                job
            };
            pool.sync_execute_for_all(job_factory);
            pool.wait_for_all_idle();
        }

        let count = counter.lock().unwrap();
        assert_eq!(*count, 1000);
    }

    #[test]
    fn test_dynamic_resize() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut pool = ThreadPool::new(Arc::clone(&stop));

        // Initial size
        pool.resize(5);
        assert_eq!(pool.workers.len(), 4);

        // Increase size
        pool.resize(10);
        assert_eq!(pool.workers.len(), 9);

        // Decrease size
        pool.resize(2);
        assert_eq!(pool.workers.len(), 1);

        // Resize to 1
        pool.resize(1);
        assert_eq!(pool.workers.len(), 0);

        // Resize to 0 (should be 1)
        pool.resize(0);
        assert_eq!(pool.workers.len(), 0);
    }

    #[test]
    fn test_execute_on_first_available() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut pool = ThreadPool::new(Arc::clone(&stop));
        pool.resize(3); // Main + 2 workers = 3 threads total

        let job_duration = std::time::Duration::from_millis(50);
        let jobs_executed = Arc::new(AtomicU64::new(0));

        // Make all threads busy initially
        let initial_job_factory = || {
            let jobs_executed_clone = Arc::clone(&jobs_executed);
            let job: BoxedJob = Box::new(move |_, _| {
                std::thread::sleep(job_duration);
                jobs_executed_clone.fetch_add(1, Ordering::SeqCst);
            });
            job
        };
        pool.sync_execute_for_all(initial_job_factory); // Start jobs on all 3 threads

        // Now, submit new jobs using execute_on_first_available while others are busy
        let num_extra_jobs = 15;
        for i in 0..num_extra_jobs {
            let pool_ref = &pool; // Need a reference to satisfy borrow checker in thread
            let jobs_executed_clone = Arc::clone(&jobs_executed);
            let job: BoxedJob = Box::new(move |worker, _| {
                println!(
                    "Extra job {} running on thread {}",
                    i,
                    worker.lock().unwrap().thread_id
                );
                std::thread::sleep(job_duration / 2); // Shorter job
                jobs_executed_clone.fetch_add(1, Ordering::SeqCst);
            });

            // This call will block until one of the initial 3 jobs finishes
            pool_ref.execute_on_first_available(job);
            println!("Extra job {} submitted.", i);
            // Small sleep to potentially allow threads to become free and test contention
            std::thread::sleep(std::time::Duration::from_millis(5));
        }

        // Wait for all pool threads to become idle (ensures all submitted jobs are done)
        pool.wait_for_all_idle();

        // Verify total jobs executed: 3 initial + 5 extra
        assert_eq!(
            jobs_executed.load(Ordering::SeqCst),
            3 + num_extra_jobs as u64
        );
        println!("All jobs completed.");
    }
}
