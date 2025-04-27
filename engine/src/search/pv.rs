use chess::{Move, board::MAX_MOVES};

#[derive(Debug, Clone)]
pub struct PVLine {
    pub moves: [Move; MAX_MOVES],
    pub length: usize,
}

impl Default for PVLine {
    fn default() -> Self {
        Self {
            moves: [Move::NONE; MAX_MOVES],
            length: 0,
        }
    }
}

impl PVLine {
    pub fn clear(&mut self) {
        self.length = 0;
    }
}
