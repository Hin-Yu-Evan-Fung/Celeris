mod clock;
mod pv;
mod search;
mod tt;

pub use clock::Clock;
pub use pv::PVLine;
pub use search::SearchStats;
pub(crate) use search::SearchWorker;
pub use tt::TT;

const MIN_DEPTH: usize = 4;

pub trait NodeType {
    const PV: bool;
    const ROOT: bool;
    type Next: NodeType;
}

pub struct PV;

pub struct Root;

pub struct NonPV;

impl NodeType for Root {
    const PV: bool = true;
    const ROOT: bool = true;
    type Next = PV;
}

impl NodeType for PV {
    const PV: bool = true;
    const ROOT: bool = false;
    type Next = Self;
}

impl NodeType for NonPV {
    const PV: bool = false;
    const ROOT: bool = false;
    type Next = Self;
}
