mod clock;
mod pv;
mod search;
mod stack;

pub use clock::Clock;
pub use pv::PVLine;
pub use search::SearchWorker;
pub(crate) use stack::SearchStack;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeType {
    PV,
    Root,
}

pub trait NodeTypeTrait {
    fn node_type() -> NodeType;
}

pub struct PVNode;

pub struct RootNode;

impl NodeTypeTrait for PVNode {
    fn node_type() -> NodeType {
        NodeType::PV
    }
}

impl NodeTypeTrait for RootNode {
    fn node_type() -> NodeType {
        NodeType::Root
    }
}
