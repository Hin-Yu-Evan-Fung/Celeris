mod eval;

pub use eval::evaluate_nnue;

use crate::constants::*;
use chess::impl_ari_ops;

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Eval(pub i16);

impl_ari_ops!(Eval);

impl Eval {
    pub const ZERO: Eval = Eval(0);
    pub const DRAW: Eval = Eval(0);

    pub const INFINITY: Eval = Eval(32001);

    pub const MATE: Eval = Eval(32000);

    pub const MATE_BOUND: Eval = Eval(Self::MATE.0 - MAX_DEPTH as i16);

    pub fn mated_in(ply: u16) -> Eval {
        -Self::MATE + Eval(ply as i16)
    }

    pub fn mate_in(ply: u16) -> Eval {
        Self::MATE - Eval(ply as i16)
    }

    pub fn from_tt(&self, ply: u16) -> Eval {
        let ply = Eval(ply as i16);

        if *self >= Self::MATE_BOUND {
            *self - ply
        } else if *self <= -Self::MATE_BOUND {
            *self + ply
        } else {
            *self
        }
    }

    pub fn to_tt(&self, ply: u16) -> Eval {
        let ply = Eval(ply as i16);

        if *self >= Self::MATE_BOUND {
            *self + ply
        } else if *self <= -Self::MATE_BOUND {
            *self - ply
        } else {
            *self
        }
    }
}

impl std::ops::Neg for Eval {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Eval(-self.0)
    }
}

impl std::fmt::Display for Eval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.abs() >= Self::MATE_BOUND.0 {
            let moves_to_mate = (Self::MATE.0 - self.0.abs() + 1) / 2;
            if self.0 > 0 {
                write!(f, "mate +{}", moves_to_mate)
            } else {
                write!(f, "mate -{}", moves_to_mate)
            }
        } else {
            write!(f, "cp {}", self.0)
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Score(pub Eval, pub Eval);

macro_rules! S {
    ($mg:expr, $eg:expr) => {
        Score(Eval($mg), Eval($eg))
    };
}

pub(crate) use S;

impl Score {
    pub const ZERO: Score = S!(0, 0);
}

impl std::ops::Add for Score {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl std::ops::Sub for Score {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0, self.1 - rhs.1)
    }
}

impl std::ops::Mul<i16> for Score {
    type Output = Self;

    fn mul(self, rhs: i16) -> Self::Output {
        Self(self.0 * Eval(rhs), self.1 * Eval(rhs))
    }
}

impl std::ops::AddAssign for Score {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

impl std::ops::SubAssign for Score {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
        self.1 -= rhs.1;
    }
}

impl std::ops::MulAssign<i16> for Score {
    fn mul_assign(&mut self, rhs: i16) {
        self.0 *= Eval(rhs);
        self.1 *= Eval(rhs);
    }
}

impl std::ops::Neg for Score {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0, -self.1)
    }
}
