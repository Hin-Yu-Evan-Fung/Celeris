use std::{
    sync::{
        Arc, Condvar, Mutex,
        atomic::AtomicBool,
        mpsc::{Sender, channel},
    },
    thread::{self, JoinHandle},
};

use crate::search::SearchWorker;

// --- Job Definition ---

/// Type alias for a job closure.
/// Takes the worker state and global stop signal.
/// Must be Send + 'static to be used across threads.
pub type BoxedJob = Box<dyn FnOnce(&Arc<Mutex<SearchWorker>>, &Arc<AtomicBool>) + Send + 'static>;

// --- Message Definition ---
enum Message {
    NewJob(BoxedJob),
    Terminate,
}

// --- Thread State Definition ---
#[derive(PartialEq)]
enum ThreadState {
    Idle,
    Busy,
}

// --- Thread Definition ---

/// Manages a single worker thread, its state, and communication.
pub struct WorkerThread {
    id: usize,
    /// Holds state specific to the work being done (e.g., search state).
    worker: Arc<Mutex<SearchWorker>>,
    /// Handle to the spawned OS thread. `None` if not started or after joining.
    handle: Option<JoinHandle<()>>,
    /// Sender to allow synchronisation between the thread struct and the worker loop
    sender: Sender<Message>,
    /// Shared signal for interrupting long-running jobs.
    stop: Arc<AtomicBool>,
    /// Condvar for thread synchronisation
    sync: Arc<(Mutex<ThreadState>, Condvar)>,
}

impl WorkerThread {
    /// Creates a new Thread controller. The OS thread is not spawned yet.
    pub fn new(id: usize, stop: Arc<AtomicBool>) -> Self {
        let (sender, receiver) = channel();
        let receiver = Arc::new(Mutex::new(receiver));

        let worker = Arc::new(Mutex::new(SearchWorker::new(id)));
        let sync = Arc::new((Mutex::new(ThreadState::Idle), Condvar::new()));

        let worker_clone = Arc::clone(&worker);
        let stop_clone = Arc::clone(&stop);
        let sync_clone = Arc::clone(&sync);

        let handle = Some(thread::spawn(move || {
            loop {
                if let Ok(message) = receiver.lock().unwrap().recv() {
                    match message {
                        Message::NewJob(job) => {
                            // Run the job
                            job(&worker_clone, &stop_clone);

                            // Reset done flag to false before starting a new job
                            let &(ref lock, ref cvar) = &*sync_clone;
                            let mut state = lock.lock().unwrap();

                            // Job is done, set the flag to true
                            *state = ThreadState::Idle;

                            cvar.notify_one();
                        }
                        Message::Terminate => break,
                    }
                } else {
                    // Sender disconnected - ThreadPool is dropped so the threads should shutdown as well
                    break;
                }
            }
        }));

        Self {
            id,
            worker,
            handle,
            sender,
            stop,
            sync,
        }
    }

    pub fn execute(&self, job: BoxedJob) {
        let &(ref lock, _) = &*self.sync;
        let mut state = lock.lock().unwrap();
        *state = ThreadState::Busy;

        self.sender.send(Message::NewJob(job)).unwrap();
    }

    /// Tell all running jobs to stop
    pub fn stop(&self) {
        // self.sender.send(Message::Terminate).unwrap();
        self.stop.store(true, std::sync::atomic::Ordering::Relaxed);
    }

    pub fn shutdown(&mut self) {
        self.stop();

        // Thread has already shutdown
        if self.handle.is_none() {
            return;
        }

        self.wait_for_idle();

        self.sender.send(Message::Terminate).unwrap();
        if let Some(handle) = self.handle.take() {
            handle.join().unwrap();
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn is_main_thread(&self) -> bool {
        self.id() == 0
    }

    pub fn is_idle(&self) -> bool {
        let &(ref lock, _) = &*self.sync;
        let state = lock.lock().unwrap();
        *state == ThreadState::Idle
    }

    pub fn wait_for_idle(&self) {
        let &(ref lock, ref cvar) = &*self.sync;
        let mut state = lock.lock().unwrap();
        while *state != ThreadState::Idle {
            state = cvar.wait(state).unwrap(); // Wait until notified
        }
    }
}

impl Drop for WorkerThread {
    fn drop(&mut self) {
        self.shutdown();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_single_job_transition() {
        let stop = Arc::new(AtomicBool::new(false));
        let thread = WorkerThread::new(0, Arc::clone(&stop));

        thread.execute(Box::new(|_worker, _stop| {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }));

        // Should be Working initially
        assert!(!thread.is_idle());

        // Wait for it to go back to Idle
        thread.wait_for_idle();
        assert!(thread.is_idle());
    }

    #[test]
    fn test_multiple_jobs() {
        let stop = Arc::new(AtomicBool::new(false));
        let thread = WorkerThread::new(1, Arc::clone(&stop));

        for _ in 0..5 {
            thread.execute(Box::new(|_worker, _stop| {
                std::thread::sleep(std::time::Duration::from_millis(5));
            }));

            thread.wait_for_idle();
            assert!(thread.is_idle());
        }
    }

    #[test]
    fn test_stop_signal_respected() {
        let stop = Arc::new(AtomicBool::new(false));
        let worker = WorkerThread::new(2, Arc::clone(&stop));

        worker.execute(Box::new(|_worker, stop| {
            while !stop.load(std::sync::atomic::Ordering::Relaxed) {
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
        }));

        // Give it a bit of time to start running
        std::thread::sleep(std::time::Duration::from_millis(10));
        stop.store(true, std::sync::atomic::Ordering::Relaxed);

        worker.wait_for_idle();
        assert!(worker.is_idle());
    }

    #[test]
    fn test_clean_shutdown() {
        let stop = Arc::new(AtomicBool::new(false));
        let mut worker = WorkerThread::new(3, Arc::clone(&stop));

        worker.execute(Box::new(|_worker, _stop| {
            std::thread::sleep(std::time::Duration::from_millis(5));
        }));

        worker.wait_for_idle();

        // Should shutdown without panic
        worker.shutdown();
    }

    #[test]
    fn test_spurious_wakeup_tolerance() {
        let stop = Arc::new(AtomicBool::new(false));
        let worker = WorkerThread::new(4, Arc::clone(&stop));

        worker.execute(Box::new(|_worker, _stop| {
            std::thread::sleep(std::time::Duration::from_millis(50));
        }));

        // Early wait, job is not finished yet
        worker.wait_for_idle();

        // After waiting properly, should be idle
        assert!(worker.is_idle());
    }

    #[test]
    fn test_stress_many_jobs() {
        let stop = Arc::new(AtomicBool::new(false));
        let worker = WorkerThread::new(5, Arc::clone(&stop));

        let counter = Arc::new(std::sync::Mutex::new(0));

        for _ in 0..100 {
            let counter_clone = Arc::clone(&counter);
            worker.execute(Box::new(move |_worker, _stop| {
                let mut count = counter_clone.lock().unwrap();
                *count += 1;
            }));
            worker.wait_for_idle();
        }

        let count = counter.lock().unwrap();
        assert_eq!(*count, 100);
    }
}
