use chess::{Move, board::MAX_MOVES};

#[derive(Debug, Clone, Copy)]
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

impl std::ops::Index<usize> for PVLine {
    type Output = Move;

    fn index(&self, index: usize) -> &Self::Output {
        &self.moves[index]
    }
}

impl std::fmt::Display for PVLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::from("pv");
        for m in &self.moves[0..self.length] {
            s.push_str(&format!(" {m}"));
        }

        write!(f, "{}", s)
    }
}

impl PVLine {
    pub fn update_line(&mut self, move_: Move, old: &Self) {
        self.length = old.length + 1;
        self.moves[0] = move_;
        self.moves[1..=old.length].copy_from_slice(&old.moves[..old.length]);
    }

    pub fn clear(&mut self) {
        self.length = 0;
    }

    pub fn load(&mut self, move_: Move, other: &Self) {
        self.clear();
        self.moves[0] = move_;
        self.moves[1..=other.length].copy_from_slice(&other.moves[..other.length]);
        self.length = other.length + 1;
    }
}
