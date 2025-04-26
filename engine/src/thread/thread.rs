// use crate::search::SearchWorker;
// use std::{
//     error::Error,
//     fmt, mem,
//     sync::{Arc, Condvar, Mutex, MutexGuard, atomic::AtomicBool},
//     thread::{self, JoinHandle},
// };

// // --- Job Definition ---

// /// Type alias for a job closure.
// /// Takes the worker state and global stop signal.
// /// Must be Send + 'static to be used across threads.
// pub type Job = Box<dyn FnOnce(Arc<Mutex<SearchWorker>>, Arc<AtomicBool>) + Send + 'static>;

// // --- Thread Errors ---

// /// Represents an error that can occur during thread operations.
// #[derive(Debug)]
// pub enum ThreadError {
//     /// Failed to acquire a lock (mutex poisoned).
//     LockPoisoned(String),
//     /// Failed to join the OS thread (e.g., panicked or handle missing).
//     JoinFailed(String),
//     /// Failed to start the thread (e.g., already started).
//     StartFailed(String),
//     /// Failed to assign a job (e.g., thread not idle or lock poisoned).
//     AssignFailed(String),
//     /// Failed to signal termination (e.g., already terminating or lock poisoned).
//     TerminateFailed(String),
// }

// impl fmt::Display for ThreadError {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             ThreadError::LockPoisoned(msg) => write!(f, "Mutex lock poisoned: {msg}"),
//             ThreadError::JoinFailed(msg) => write!(f, "Thread join failed: {msg}"),
//             ThreadError::StartFailed(msg) => write!(f, "Thread start failed: {msg}"),
//             ThreadError::AssignFailed(msg) => write!(f, "Thread assign failed: {msg}"),
//             ThreadError::TerminateFailed(msg) => write!(f, "Thread terminate failed: {msg}"),
//         }
//     }
// }

// impl Error for ThreadError {}

// // --- Thread State ---

// /// Represents the internal state of the worker thread's control loop.
// enum State {
//     /// Waiting for a job.
//     Idle,
//     /// Currently assigned a job closure to perform.
//     Working(Job),
//     /// Signaled to terminate the worker loop.
//     Terminate,
// }

// // Manual Debug implementation to avoid printing closure details.
// impl fmt::Debug for State {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             State::Idle => write!(f, "Idle"),
//             State::Working(_) => write!(f, "Working(Job)"),
//             State::Terminate => write!(f, "Terminate"),
//         }
//     }
// }

// /// Manages a single worker thread, its state, and communication.
// pub struct Thread {
//     id: usize,
//     /// Holds state specific to the work being done (e.g., search state).
//     worker_state: Arc<Mutex<SearchWorker>>,
//     /// Controls the thread's lifecycle and job assignment.
//     sync: Arc<(Mutex<State>, Condvar)>,
//     /// Handle to the spawned OS thread. `None` if not started or after joining.
//     handle: Option<JoinHandle<()>>,
//     /// Shared signal for interrupting long-running jobs.
//     global_stop: Arc<AtomicBool>,
// }

// impl Thread {
//     /// Creates a new Thread controller. The OS thread is not spawned yet.
//     pub fn new(id: usize, global_stop: Arc<AtomicBool>) -> Self {
//         let worker_state = Arc::new(Mutex::new(SearchWorker::new(id)));
//         let sync = Arc::new((Mutex::new(State::Idle), Condvar::new()));
//         Self {
//             id,
//             worker_state,
//             sync,
//             handle: None,
//             global_stop,
//         }
//     }

//     /// Spawns the OS thread and starts its worker loop.
//     /// Returns an error if the thread was already started.
//     pub fn start(&mut self) -> Result<(), ThreadError> {
//         if self.handle.is_some() {
//             return Err(ThreadError::StartFailed(format!(
//                 "Thread {} already started",
//                 self.id
//             )));
//         }

//         let worker_state_clone = Arc::clone(&self.worker_state);
//         let sync_clone = Arc::clone(&self.sync);
//         let global_stop_clone = Arc::clone(&self.global_stop);
//         let thread_id = self.id;

//         let handle = thread::spawn(move || {
//             Self::worker_loop(thread_id, worker_state_clone, sync_clone, global_stop_clone)
//         });

//         self.handle = Some(handle);
//         Ok(())
//     }

//     /// The main loop executed by the spawned OS worker thread.
//     /// Waits for jobs, executes them, and handles termination signals.
//     fn worker_loop(
//         thread_id: usize,
//         worker_state: Arc<Mutex<SearchWorker>>,
//         sync: Arc<(Mutex<State>, Condvar)>,
//         global_stop: Arc<AtomicBool>,
//     ) {
//         let (lock, cvar) = &*sync;

//         loop {
//             // --- Wait for work (state is not Idle) ---
//             // This returns the guard when state is no longer Idle
//             let mut state_guard =
//                 match Self::wait_while_state(&sync, |state| matches!(state, State::Idle)) {
//                     Ok(guard) => guard,
//                     Err(e) => {
//                         // Handle poison error from wait_while_state
//                         eprintln!("FATAL: Thread {thread_id} encountered error during wait: {e}");
//                         panic!("Thread {thread_id} failed during wait: {e}"); // Panic to ensure visibility
//                     }
//                 };

//             // --- Woken Up: State is Working or Terminate ---
//             // Check for termination signal first.
//             if matches!(*state_guard, State::Terminate) {
//                 drop(state_guard); // Release lock before breaking
//                 break; // Exit the loop
//             }

//             // State must be Working. Extract the job using mem::replace.
//             // Replace with Idle temporarily while the job runs outside the lock.
//             let job_to_run = match mem::replace(&mut *state_guard, State::Idle) {
//                 State::Working(job) => job,
//                 // Should be unreachable due to wait_while and Terminate check
//                 _ => unreachable!("State should be Working or Terminate after wait_while"),
//             };

//             // Release the lock *before* executing the potentially long job.
//             drop(state_guard);

//             // --- Execute Job ---
//             job_to_run(worker_state.clone(), global_stop.clone());

//             // --- Job Finished: Notify Waiters ---
//             // Re-acquire lock to potentially update state (though it's already Idle)
//             // and notify any threads waiting in wait_for_job_finish.
//             let final_state_guard = lock.lock().unwrap_or_else(|poisoned| {
//                 eprintln!(
//                     "FATAL: Thread {thread_id} sync mutex poisoned after job! Error: {poisoned}"
//                 );
//                 panic!("Thread {thread_id} sync mutex poisoned after job!");
//             });

//             // No state change needed here as we replaced with Idle earlier.
//             // Just notify waiters.
//             cvar.notify_all();
//             drop(final_state_guard);
//         }
//         // Worker loop finished (only happens on Terminate)
//     }

//     // --- Helper Functions for State Modification and Waiting ---

//     /// Locks the state mutex and executes the provided closure.
//     /// Handles potential lock poisoning.
//     fn modify_state<F, R>(sync: &Arc<(Mutex<State>, Condvar)>, action: F) -> Result<R, ThreadError>
//     where
//         F: FnOnce(&mut State, &Condvar) -> Result<R, ThreadError>,
//     {
//         let (lock, cvar) = &**sync;
//         let mut state_guard = lock
//             .lock()
//             .map_err(|e| ThreadError::LockPoisoned(format!("modify_state lock failed: {e}")))?;
//         action(&mut *state_guard, cvar)
//     }

//     /// Locks the state mutex and waits efficiently while the predicate is true.
//     /// Returns the `MutexGuard` when the predicate becomes false.
//     /// Handles potential lock poisoning during locking or waiting.
//     fn wait_while_state<'a, P>(
//         sync: &'a Arc<(Mutex<State>, Condvar)>,
//         predicate: P,
//     ) -> Result<MutexGuard<'a, State>, ThreadError>
//     where
//         P: FnMut(&mut State) -> bool, // Predicate takes &mut State for wait_while
//     {
//         let (lock, cvar) = &**sync;
//         let state_guard = lock
//             .lock()
//             .map_err(|e| ThreadError::LockPoisoned(format!("wait_while_state lock failed: {e}")))?;

//         // Use wait_while, which handles spurious wakeups correctly and returns the guard
//         cvar.wait_while(state_guard, predicate)
//             .map_err(|e| ThreadError::LockPoisoned(format!("wait_while_state wait failed: {e}")))
//     }

//     /// Assigns a job closure to this thread if it's currently idle.
//     ///
//     /// # Errors
//     /// Returns `ThreadError::LockPoisoned` if the state mutex is poisoned.
//     /// Returns `ThreadError::AssignFailed` if the thread is not currently idle.
//     pub fn assign_job(&self, job: Job) -> Result<(), ThreadError> {
//         let (lock, cvar) = &*self.sync;
//         let mut state_guard = lock
//             .lock()
//             .map_err(|e| ThreadError::LockPoisoned(format!("assign_job lock failed: {e}")))?;

//         if matches!(*state_guard, State::Idle) {
//             *state_guard = State::Working(job);
//             cvar.notify_one(); // Wake up the worker thread
//             Ok(())
//         } else {
//             Err(ThreadError::AssignFailed(format!(
//                 "Thread {} not Idle (state: {:?})",
//                 self.id, *state_guard
//             )))
//         }
//     }

//     /// Waits efficiently until the thread finishes its current job and becomes Idle or Terminated.
//     ///
//     /// # Errors
//     /// Returns `ThreadError::LockPoisoned` if the state mutex is poisoned during locking or waiting.
//     pub fn wait_for_job_finish(&self) -> Result<(), ThreadError> {
//         let (lock, cvar) = &*self.sync;
//         let mut state_guard = lock.lock().map_err(|e| {
//             ThreadError::LockPoisoned(format!("wait_for_job_finish lock failed: {e}"))
//         })?;

//         // Wait efficiently while the state is Working
//         state_guard = cvar
//             .wait_while(state_guard, |state| matches!(state, State::Working(_)))
//             .map_err(|e| {
//                 ThreadError::LockPoisoned(format!("wait_for_job_finish wait failed: {e}"))
//             })?;

//         // When wait_while returns Ok, state is no longer Working (must be Idle or Terminate)
//         Ok(())
//     }

//     /// Signals this thread to terminate its worker loop *after* finishing its current job.
//     /// Private helper for `wait_to_terminate`.
//     fn signal_terminate(&self) -> Result<(), ThreadError> {
//         let (lock, cvar) = &*self.sync;
//         let mut state_guard = lock
//             .lock()
//             .map_err(|e| ThreadError::LockPoisoned(format!("signal_terminate lock failed: {e}")))?;

//         if matches!(*state_guard, State::Terminate) {
//             // Return error if already signaled
//             return Err(ThreadError::TerminateFailed(format!(
//                 "Thread {} already terminating",
//                 self.id
//             )));
//         }

//         *state_guard = State::Terminate;
//         cvar.notify_one(); // Wake up the thread if it's idle, so it sees Terminate
//         Ok(())
//     }

//     /// Waits for the OS thread handle to finish execution.
//     /// Private helper for `wait_to_terminate`.
//     fn join(&mut self) -> Result<(), ThreadError> {
//         match self.handle.take() {
//             Some(handle) => handle.join().map_err(|payload| {
//                 ThreadError::JoinFailed(format!("Thread {} panicked: {:?}", self.id, payload))
//             }),
//             None => Err(ThreadError::JoinFailed(format!(
//                 "Thread {} handle missing",
//                 self.id
//             ))),
//         }
//     }

//     /// Signals the thread to terminate and waits for the OS thread to fully exit.
//     /// Prints errors encountered during signaling or joining to stderr.
//     /// This is the primary public method for shutting down a thread gracefully.
//     pub fn wait_to_terminate(&mut self) {
//         if let Err(e) = self.signal_terminate() {
//             // Log error, but proceed to join anyway if possible
//             eprintln!("Error signaling terminate for thread {}: {}", self.id, e);
//         }
//         if let Err(e) = self.join() {
//             eprintln!("Error joining thread {}: {}", self.id, e);
//         }
//     }

//     /// Checks if the thread is currently idle by attempting to lock its state.
//     /// Returns `false` if the lock is poisoned.
//     pub fn is_idle(&self) -> bool {
//         match self.sync.0.lock() {
//             // Access tuple element directly
//             Ok(state_guard) => matches!(*state_guard, State::Idle),
//             Err(poisoned) => {
//                 eprintln!(
//                     "Error checking idle status for thread {}: mutex poisoned: {}",
//                     self.id, poisoned
//                 );
//                 false // Treat poisoned state as not reliably idle
//             }
//         }
//     }
// }

// impl Drop for Thread {
//     /// Ensures the associated OS thread is signaled to terminate and joined when
//     /// the `Thread` controller goes out of scope.
//     fn drop(&mut self) {
//         // Only attempt cleanup if the thread was actually started.
//         if self.handle.is_some() {
//             self.wait_to_terminate();
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use std::{sync::atomic::Ordering, time::Duration};

//     #[test]
//     fn test_thread_job_execution_and_wait() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut thread = Thread::new(1, stop.clone());
//         thread.start().expect("Failed to start thread");

//         let job_finished_flag = Arc::new(AtomicBool::new(false));
//         let flag_clone = job_finished_flag.clone();
//         let job: Job = Box::new(move |_, _| {
//             thread::sleep(Duration::from_millis(50)); // Simulate work
//             flag_clone.store(true, Ordering::SeqCst);
//         });

//         thread.assign_job(job).expect("Failed to assign job");
//         assert!(!thread.is_idle());

//         thread.wait_for_job_finish().expect("Wait failed");

//         assert!(thread.is_idle());
//         assert!(job_finished_flag.load(Ordering::SeqCst));

//         // Drop handles cleanup via wait_to_terminate
//     }

//     #[test]
//     fn test_thread_stop_during_search() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut thread = Thread::new(2, stop.clone());
//         thread.start().expect("Failed to start thread");

//         let job: Job = Box::new(|_, stop_signal| {
//             for _ in 0..10 {
//                 if stop_signal.load(Ordering::Relaxed) {
//                     println!("Test job saw stop signal");
//                     return;
//                 }
//                 thread::sleep(Duration::from_millis(20));
//             }
//         });
//         thread.assign_job(job).expect("Failed to assign job");
//         assert!(!thread.is_idle());

//         thread::sleep(Duration::from_millis(50));
//         println!("TEST: Signaling global stop");
//         stop.store(true, Ordering::Relaxed);

//         thread.wait_for_job_finish().expect("Wait failed");
//         assert!(thread.is_idle());

//         // Drop handles cleanup
//     }

//     #[test]
//     fn test_assign_job_when_busy() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut thread = Thread::new(3, stop.clone());
//         thread.start().expect("Failed to start thread");

//         let long_job: Job = Box::new(|_, _| {
//             thread::sleep(Duration::from_millis(100));
//         });
//         thread
//             .assign_job(long_job)
//             .expect("Assign first job failed");
//         assert!(!thread.is_idle());

//         let short_job: Job = Box::new(|_, _| {
//             println!("Short job ran");
//         });
//         let result = thread.assign_job(short_job);

//         assert!(result.is_err());
//         assert!(matches!(result, Err(ThreadError::AssignFailed(_))));

//         thread
//             .wait_for_job_finish()
//             .expect("Wait for long job failed");
//         assert!(thread.is_idle());

//         let short_job_again: Job = Box::new(|_, _| {
//             println!("Short job ran again");
//         });
//         thread
//             .assign_job(short_job_again)
//             .expect("Assign second time failed");

//         thread
//             .wait_for_job_finish()
//             .expect("Wait for second short job failed");
//         assert!(thread.is_idle());

//         // Drop handles cleanup
//     }

//     #[test]
//     fn test_double_start_fails() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut thread = Thread::new(4, stop.clone());
//         assert!(thread.start().is_ok());
//         let result = thread.start(); // Try starting again
//         assert!(result.is_err());
//         assert!(matches!(result, Err(ThreadError::StartFailed(_))));
//         // Drop handles cleanup
//     }

//     #[test]
//     fn test_terminate_signal() {
//         let stop = Arc::new(AtomicBool::new(false));
//         let mut thread = Thread::new(5, stop.clone());
//         thread.start().expect("Start failed");

//         // Assign a job that waits a bit
//         let job_started = Arc::new(AtomicBool::new(false));
//         let started_clone = job_started.clone();
//         let job: Job = Box::new(move |_, _| {
//             started_clone.store(true, Ordering::SeqCst);
//             thread::sleep(Duration::from_millis(200)); // Long sleep
//         });
//         thread.assign_job(job).expect("Assign failed");

//         // Wait for job to start
//         while !job_started.load(Ordering::Relaxed) {
//             thread::sleep(Duration::from_millis(5));
//         }

//         // Signal terminate while job is running
//         thread.signal_terminate().expect("Signal terminate failed");

//         // Try signaling again, should fail
//         let terminate_again_result = thread.signal_terminate();
//         assert!(terminate_again_result.is_err());
//         assert!(matches!(
//             terminate_again_result,
//             Err(ThreadError::TerminateFailed(_))
//         ));

//         // Join should wait until the job finishes and the loop exits
//         thread.join().expect("Join failed");
//         // Handle is now None, drop won't try to join again
//     }
// }
