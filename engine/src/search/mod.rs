mod clock;
mod pv;
mod search;
mod stack;
mod tt;

pub use clock::Clock;
pub use pv::PVLine;
pub use search::SearchStats;
pub(crate) use search::SearchWorker;
pub(crate) use stack::SearchStack;
pub use tt::TT;

const MIN_DEPTH: usize = 4;

pub trait NodeTypeTrait {
    const PV: bool;
    const ROOT: bool;
}

pub struct PV;

pub struct Root;

pub struct NonPV;

impl NodeTypeTrait for PV {
    const PV: bool = true;
    const ROOT: bool = false;
}

impl NodeTypeTrait for Root {
    const PV: bool = true;
    const ROOT: bool = true;
}

impl NodeTypeTrait for NonPV {
    const PV: bool = false;
    const ROOT: bool = false;
}
