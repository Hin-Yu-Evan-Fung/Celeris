use sophos::movegen::init_all_tables;

fn main() {
    use std::time::Instant;
    // Time the initialization of the move generation tables
    let start = Instant::now();
    // Initialize the move generation tables
    init_all_tables();
    let duration = start.elapsed();
    println!(
        "Time taken to initialize the move generation tables: {:?}",
        duration
    );
}
