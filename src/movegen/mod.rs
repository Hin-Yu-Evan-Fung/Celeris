pub mod lookup;
pub mod magic;
pub mod magic_numbers;

pub use lookup::{init_all_tables, leaper_attack, pawn_attack};
pub use magic::{SliderAttackTable, attacks_on_the_fly, get_magic_tables};
pub use magic_numbers::{BISHOP_MAGICS, ROOK_MAGICS};
