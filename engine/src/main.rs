use engine::{UCI, run_bench};
use std::env::args;

const DEFAULT_CMD_BENCH_DEPTH: usize = 13; // Default depth for command-line bench

fn main() {
    let mut cli_args = args();
    cli_args.next(); // Skip the program name

    match cli_args.next().as_deref() {
        Some("bench") => {
            let depth = cli_args
                .next()
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or_else(|| {
                    println!("info string No depth specified for bench or invalid format, using default depth {}.", DEFAULT_CMD_BENCH_DEPTH); 
                DEFAULT_CMD_BENCH_DEPTH
                });
            if depth == 0 {
                // Or some other validation like depth > MAX_PLY
                println!(
                    "info string Invalid depth 0. Using default depth {}.",
                    DEFAULT_CMD_BENCH_DEPTH
                );
                run_bench(DEFAULT_CMD_BENCH_DEPTH);
            } else {
                run_bench(depth);
            }
        }
        _ => UCI::init(),
    }
}
