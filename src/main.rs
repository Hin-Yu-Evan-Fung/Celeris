use std::env::current_exe;

use sophos::core::*;
use sophos::movegen::lookup::*;

fn main() {
    println!("{}", std::mem::size_of::<Board>());
}
