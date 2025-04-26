use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64},
    },
    time::{Duration, Instant},
};

use crate::engine::TimeControl;

#[derive(Clone, Debug)]
pub struct Clock {
    global_stop: Arc<AtomicBool>,
    global_nodes: Arc<AtomicU64>,
    time_control: TimeControl,
    start_time: Instant,
    opt_time: Duration,
    max_time: Duration,
}

impl Clock {
    pub const CHECK_FREQUENCY: usize = 2048;
    pub const OVERHEAD: usize = 100;

    pub fn new(
        global_stop: Arc<AtomicBool>,
        global_nodes: Arc<AtomicU64>,
        time_control: TimeControl,
    ) -> Self {
        Self {
            global_stop,
            global_nodes,
            time_control,
            start_time: Instant::now(),
            opt_time: Duration::ZERO,
            max_time: Duration::ZERO,
        }
    }

    pub fn start(&mut self) {
        self.start_time = Instant::now();
    }

    pub fn get_elapsed_time(&self) -> Duration {
        self.start_time.elapsed()
    }
}
