// use std::sync::{
//     Arc,
//     atomic::{AtomicBool, AtomicU64, Ordering},
// };

// use super::{Job, Thread, ThreadError};

// pub struct ThreadPool {
//     main_thread: Thread,
//     workers: Vec<Thread>,
//     global_stop: Arc<AtomicBool>,
//     global_nodes: Arc<AtomicU64>,
// }

// impl ThreadPool {
//     /// Creates a new ThreadPool with the specified number of threads.
//     /// Threads are initialized but not yet started.
//     ///
//     /// # Arguments
//     ///
//     /// * `num_threads` - The total number of threads to create in the pool. Must be at least 1.
//     pub fn new(global_stop: Arc<AtomicBool>) -> Self {
//         let global_nodes = Arc::new(AtomicU64::new(0));

//         Self {
//             main_thread: Thread::new(0, global_stop.clone()),
//             workers: Vec::new(),
//             global_stop,
//             global_nodes,
//         }
//     }

//     /// Resizes the thread pool.
//     /// If growing, new threads are created but not started automatically.
//     /// If shrinking, excess threads are dropped (which triggers their Drop impl for cleanup).
//     pub fn resize(&mut self, new_size: usize) {
//         if new_size < 1 {
//             eprintln!("Cannot resize thread pool to less than 1 thread!");
//             return;
//         }

//         let current_size = self.workers.len() + 1;

//         if new_size > current_size {
//             // Grow: Add new threads
//             for id in current_size..new_size {
//                 let thread = Thread::new(id, self.global_stop.clone());
//                 // Note: New threads need to be started explicitly via start_all or individually
//                 self.workers.push(thread);
//             }
//         } else if new_size < current_size {
//             // Shrink: Truncate the vector. Dropped threads will clean themselves up.
//             self.workers.truncate(new_size - 1);
//         }
//         // If new_size == current_size, do nothing.
//     }

//     /// Starts all the OS threads managed by this pool that haven't been started yet.
//     ///
//     /// Returns `Ok(())` if all threads start successfully or were already running.
//     /// Returns the first `ThreadError` encountered if any thread fails to start.
//     pub fn start_all(&mut self) {
//         self.main_thread.start();

//         for thread in self.workers.iter_mut() {
//             // Attempt to start each thread. Ignore if already started (start returns false).
//             // If start returned Result, we would handle the error here.
//             // Since it returns bool, we just call it.
//             let _ = thread.start(); // Ignore return value for now
//         }
//     }

//     /// Waits (by calling wait_for_job_finish on each thread) until all threads
//     /// in the pool finish their current job and become Idle or Killed.
//     ///
//     /// # Returns
//     /// * `Ok(())` - If all threads successfully finished their jobs.
//     /// * `Err(ThreadError)` - The first error encountered while waiting for a thread
//     ///   (e.g., LockPoisoned).
//     pub fn wait_for_all_jobs_finish(&self) -> Result<(), ThreadError> {
//         self.main_thread.wait_for_job_finish()?;

//         for thread in self.workers.iter() {
//             // Call the per-thread wait function. Propagate the first error.
//             thread.wait_for_job_finish()?;
//         }
//         Ok(()) // All threads finished successfully
//     }

//     /// Waits for all threads to become idle, then assigns a new job (created
//     /// by the factory) to *every* thread in the pool.
//     ///
//     /// # Arguments
//     ///
//     /// * `job_factory` - A function that creates a new instance of the job closure for each thread.
//     ///
//     /// # Returns
//     ///
//     /// * `Ok(())` - If all threads were idle and the job was successfully assigned to all of them.
//     /// * `Err(ThreadError)` - If waiting for idle failed (e.g. poisoning) or if any thread
//     ///   failed the assignment. Returns the first error encountered.
//     pub fn run_job_on_all_when_idle<F>(&self, job_factory: F) -> Result<(), ThreadError>
//     where
//         F: Fn() -> Job,
//     {
//         // 1. Wait until all threads report being idle. This now returns Result.
//         self.wait_for_all_jobs_finish()?; // Use the new wait function

//         // 2. Attempt to assign the job to main thread
//         let job = job_factory(); // Create a new job instance for the main thread
//         self.main_thread.assign_job(job)?;

//         // 3. Attempt to assign the job to all threads.
//         for thread in self.workers.iter() {
//             let job = job_factory(); // Create a new job instance for this thread
//             // Try to assign the job. If it fails for any thread, return the error immediately.
//             thread.assign_job(job)?; // Propagate the error if assign_job fails
//         }

//         // If the loop completes, all assignments were successful.
//         Ok(())
//     }

//     /// Sets the global stop signal to `true`.
//     /// This requests that ongoing jobs (like search) interrupt themselves.
//     fn set_stop(&self) {
//         self.global_stop.store(true, Ordering::Relaxed);
//     }

//     /// Clears the global stop signal (sets it to `false`).
//     /// Should be called before starting a new batch of work that uses the stop signal.
//     fn clear_stop(&self) {
//         self.global_stop.store(false, Ordering::Relaxed);
//     }

//     /// Signals all threads to terminate and waits for them to finish.
//     /// This is automatically called when the `ThreadPool` is dropped.
//     pub fn shutdown(&mut self) {
//         // Set stop signal to terminate current jobs
//         self.set_stop();

//         // Iterate and call wait_to_finish on each thread.
//         // wait_to_finish handles signaling terminate and joining.
//         for thread in self.workers.iter_mut() {
//             thread.wait_to_terminate();
//         }
//     }

//     /// Returns the number of threads in the pool.
//     pub fn size(&self) -> usize {
//         self.workers.len() + 1
//     }

//     /// Gets the current value of the shared node counter.
//     pub fn get_nodes(&self) -> u64 {
//         self.global_nodes.load(Ordering::Relaxed)
//     }
// }

// // Implement Drop to ensure threads are cleaned up automatically
// impl Drop for ThreadPool {
//     fn drop(&mut self) {
//         // Use the explicit shutdown method
//         self.shutdown();
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use std::sync::atomic::{AtomicBool, Ordering};
//     use std::thread;
//     use std::time::Duration;

//     #[test]
//     fn test_pool_creation_and_start() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut pool = ThreadPool::new(stop);
//         pool.resize(4);
//         assert_eq!(pool.size(), 4);
//         pool.start_all();
//         // Drop implicitly calls shutdown
//     }

//     #[test]
//     fn test_resize_pool() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut pool = ThreadPool::new(stop);
//         pool.start_all();
//         // Grow
//         pool.resize(2);
//         assert_eq!(pool.size(), 2);

//         // Grow
//         pool.resize(4);
//         assert_eq!(pool.size(), 4);
//         // Note: New threads aren't started automatically by resize in this impl
//         pool.start_all(); // Start the newly added threads

//         // Shrink
//         pool.resize(1);
//         assert_eq!(pool.size(), 1);

//         // Resize to same size
//         pool.resize(1);
//         assert_eq!(pool.size(), 1);

//         // Grow again
//         pool.resize(2);
//         assert_eq!(pool.size(), 2);
//         pool.start_all();
//     }

//     #[test]
//     fn test_run_job_on_all_and_wait() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut pool = ThreadPool::new(stop);
//         pool.start_all();

//         let jobs_ran_count = Arc::new(AtomicU64::new(0));

//         let factory = || {
//             let counter_clone = jobs_ran_count.clone();
//             let job: Job = Box::new(move |_, _| {
//                 // Simulate some work
//                 thread::sleep(Duration::from_millis(50));
//                 counter_clone.fetch_add(1, Ordering::SeqCst);
//             });
//             job
//         };

//         // Run the job on all threads
//         pool.run_job_on_all_when_idle(factory)
//             .expect("Run job failed");

//         println!("TEST: Signaling global stop");

//         // Wait for all jobs to finish using the new pool method
//         pool.wait_for_all_jobs_finish()
//             .expect("Wait for all jobs failed");

//         // Check that all threads ran the job
//         assert_eq!(jobs_ran_count.load(Ordering::SeqCst), pool.size() as u64);

//         // Check they are idle again (redundant after wait_for_all_jobs_finish, but good check)
//         assert!(pool.workers.iter().all(|t| t.is_idle()));

//         // Shutdown called implicitly by Drop
//     }

//     // Remove the test `test_run_job_on_all_when_one_busy_initially` as it relied
//     // on the spinning wait which is now replaced by the Condvar wait inside run_job_on_all_when_idle.
//     // The new `run_job_on_all_when_idle` inherently waits.
//     // We could add a test that assigns a job, calls wait_for_all_jobs_finish, then assigns another.

//     #[test]
//     fn test_wait_for_individual_jobs() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut pool = ThreadPool::new(stop);
//         pool.resize(2);
//         assert_eq!(pool.size(), 2);
//         pool.start_all();

//         let job1_finished = Arc::new(AtomicBool::new(false));
//         let job1_finished_clone = job1_finished.clone();
//         let job1: Job = Box::new(move |_, _| {
//             thread::sleep(Duration::from_millis(60));
//             job1_finished_clone.store(true, Ordering::SeqCst);
//         });

//         let job2_finished = Arc::new(AtomicBool::new(false));
//         let job2_finished_clone = job2_finished.clone();
//         let job2: Job = Box::new(move |_, _| {
//             thread::sleep(Duration::from_millis(30));
//             job2_finished_clone.store(true, Ordering::SeqCst);
//         });

//         pool.main_thread
//             .assign_job(job1)
//             .expect("Assign job 1 failed");
//         pool.workers[0]
//             .assign_job(job2)
//             .expect("Assign job 2 failed");

//         // Wait specifically for thread 1 (job 2)
//         pool.workers[0]
//             .wait_for_job_finish()
//             .expect("Wait for thread 1 failed");
//         assert!(job2_finished.load(Ordering::SeqCst));
//         assert!(!job1_finished.load(Ordering::SeqCst)); // Job 1 should still be running
//         assert!(pool.main_thread.is_idle());
//         assert!(!pool.workers[0].is_idle()); // Still idle

//         // Wait for all jobs (which now only means waiting for thread 0)
//         pool.wait_for_all_jobs_finish()
//             .expect("Wait for all jobs failed");
//         assert!(job1_finished.load(Ordering::SeqCst));
//         assert!(pool.main_thread.is_idle());
//         assert!(pool.workers[0].is_idle()); // Still idle
//     }
// }
