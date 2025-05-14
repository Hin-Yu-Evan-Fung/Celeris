use crate::init_tunables;

pub mod constants {

    use chess::board::MAX_MOVES;

    pub const NAME: &str = "Celeris";
    pub const VERSION: &str = "0.0.1";
    pub const AUTHORS: &str = "0mn1verze, TheGogy";

    pub const THREADS: usize = 1;
    pub const DEBUG: bool = true;
    pub const TT_SIZE: usize = 64;

    pub const MAX_DEPTH: usize = MAX_MOVES;
}

init_tunables! {




    pawn_val:   i32 = 82, 60, 140, 5;
    knight_val: i32 = 337, 250, 370, 5;
    bishop_val: i32 = 365, 300, 400, 5;
    rook_val:   i32 = 477, 450, 550, 5;
    queen_val:  i32 = 1025, 950, 1100, 5;


    nnue_base: i32 = 700, 600, 800, 10;
}
