use std::sync::{Arc, Mutex, mpsc};
use std::thread;

// Message to send to worker threads
enum Message<Job> {
    NewJob(Job),
    Terminate,
}

// The thread pool
struct ThreadPool<Job>
where
    Job: Send + 'static,
{
    workers: Vec<thread::JoinHandle<()>>,
    sender: mpsc::Sender<Message<Job>>,
}

impl<Job> ThreadPool<Job>
where
    Job: Send + 'static,
{
    fn new(num_threads: usize, job_handler: Arc<dyn Fn(Job) + Send + Sync>) -> Self {
        let (sender, receiver) = mpsc::channel();
        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(num_threads);

        for _ in 0..num_threads {
            let receiver = Arc::clone(&receiver);
            let handler = Arc::clone(&job_handler);

            workers.push(thread::spawn(move || {
                loop {
                    let message = receiver.lock().unwrap().recv().unwrap();
                    match message {
                        Message::NewJob(job) => {
                            handler(job);
                        }
                        Message::Terminate => break,
                    }
                }
            }));
        }

        ThreadPool { workers, sender }
    }

    fn execute(&self, job: Job) {
        self.sender.send(Message::NewJob(job)).unwrap();
    }

    fn shutdown(self) {
        for _ in &self.workers {
            self.sender.send(Message::Terminate).unwrap();
        }

        for worker in self.workers {
            worker.join().unwrap();
        }
    }
}

// Example Job Type
struct SearchJob {
    position: String, // In reality, would be your Position struct
    depth: u32,
}

fn main() {
    let handler = Arc::new(|job: SearchJob| {
        println!(
            "Searching position: {} to depth {}",
            job.position, job.depth
        );
        // here would be your search function
    });

    let pool = ThreadPool::new(4, handler);

    // Submit some example jobs
    pool.execute(SearchJob {
        position: "rnbqkb1r/pppppppp/5n2/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string(),
        depth: 6,
    });

    pool.execute(SearchJob {
        position: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string(),
        depth: 4,
    });

    // In a real engine you'd wait for search to complete properly
    std::thread::sleep(std::time::Duration::from_secs(1));

    pool.shutdown();
}
