mod command;
mod engine;
mod time;
mod uci;

pub use command::UCICommand;
pub use engine::Engine;
pub use engine::constants::*;
pub use time::TimeControl;
pub use uci::UCI;

use macros::AriOps;

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, AriOps)]
pub struct Eval(pub i16);

impl std::ops::Neg for Eval {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Eval(-self.0)
    }
}
