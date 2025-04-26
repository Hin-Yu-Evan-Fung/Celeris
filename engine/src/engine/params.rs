use chess::board::MAX_MOVES;

use macros::AriOps;

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, AriOps)]
pub struct Eval(pub i16);

impl std::ops::Neg for Eval {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Eval(-self.0)
    }
}

impl std::fmt::Display for Eval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.abs() >= MATE_BOUND.0 {
            let moves_to_mate = (MATE.0 - self.0.abs() + 1) / 2;
            if self.0 > 0 {
                write!(f, "mate {moves_to_mate}")
            } else {
                write!(f, "mate -{moves_to_mate}")
            }
        } else {
            write!(f, "cp {}", self.0)
        }
    }
}

pub const MAX_DEPTH: u16 = MAX_MOVES as u16;
pub const INFINITY: Eval = Eval(32001);
pub const MATE: Eval = Eval(32000);
pub const LONGEST_MATE: Eval = Eval(MAX_DEPTH as i16);
pub const MATE_BOUND: Eval = MATE.sub(LONGEST_MATE);
