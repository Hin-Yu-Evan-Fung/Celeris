use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicU64, Ordering},
};

use std::time::Duration;

fn main() {
    // perft_bench();
    // UCI::init();

    // let stop = Arc::new(AtomicBool::new(false));
    // let mut pool = ThreadPool::new(stop);
    // pool.start_all();

    // let jobs_ran_count = Arc::new(AtomicU64::new(0));

    // let factory = || {
    //     let counter_clone = jobs_ran_count.clone();

    //     job
    // };

    // // Run the job on all threads
    // pool.run_job_on_all_when_idle(factory)
    //     .expect("Run job failed");

    // // Wait for all jobs to finish using the new pool method
    // pool.wait_for_all_jobs_finish()
    //     .expect("Wait for all jobs failed");

    // // Check that all threads ran the job
    // assert_eq!(jobs_ran_count.load(Ordering::SeqCst), pool.size() as u64);

    // Shutdown called implicitly by Drop

    // let mut engine = Engine::new();

    // engine.set_position(Board::default());

    // engine.perft(6);
}
