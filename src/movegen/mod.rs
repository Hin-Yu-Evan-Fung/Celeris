pub mod lookup;
pub(crate) mod magic;
pub(crate) mod magic_numbers;

pub use lookup::{
    between_bb, castling_rights, check_bb, init_all_tables, leaper_attack, line_bb, pawn_attack,
    pin_bb, slider_attack,
};

pub(crate) use magic::{Magic, SliderAttackTable, attacks_on_the_fly, init_magic_struct};
