use chess::{
    Move, MoveFlag, PieceType,
    board::{Board, CaptureGen, MoveList, QuietGen},
};

use crate::{
    SearchStackEntry,
    constants::{CONT_HIST_SIZE, MVV},
    eval::Eval,
    movepick::history::Interface,
    search::SearchStats,
};

use super::{MoveStage, see::see};

pub struct MovePicker<const TACTICAL: bool> {
    pub stage: MoveStage,
    skip_quiets: bool,

    tt_move: Move,
    killers: [Move; 2],
    counter: Move,

    // Scored move list items
    move_list: MoveList,
    scores: [i32; 256],
    index: usize,

    quiet_start: usize,
    bad_cap_start: usize,
}

fn captured_value(captured: PieceType) -> i32 {
    MVV[captured.index()]
}

impl<const TACTICAL: bool> MovePicker<TACTICAL> {
    pub fn new(
        board: &Board,
        mut tt_move: Move,
        mut killers: [Move; 2],
        mut counter: Move,
    ) -> Self {
        let in_check = board.in_check();

        if TACTICAL && !in_check && board.is_capture(tt_move) {
            tt_move = Move::NONE;
        }

        // A valid killer must be legal and a non capture
        if !killers[0].is_valid() || !board.is_legal(killers[0]) {
            killers[0] = Move::NONE;
        }

        // A valid killer must be legal and a non capture
        if !killers[1].is_valid() || !board.is_legal(killers[1]) {
            killers[1] = Move::NONE;
        }

        // A valid counter must be legal and a non capture
        if !counter.is_valid() || !board.is_legal(counter) {
            counter = Move::NONE;
        }

        Self {
            stage: MoveStage::TTMove,
            skip_quiets: !in_check && TACTICAL,
            tt_move,
            killers,
            counter,
            move_list: MoveList::new(),
            scores: [0; 256],
            index: 0,
            quiet_start: 0,
            bad_cap_start: 0,
        }
    }

    pub fn skip_quiets(&mut self) {
        self.skip_quiets = true;
    }

    fn score_captures(&mut self, board: &Board, stats: &SearchStats) {
        let mut next_good_cap = 0;

        for i in 0..self.move_list.len() {
            let move_ = self.move_list[i];

            let captured = match move_.flag() {
                MoveFlag::EPCapture => PieceType::Pawn,
                _ => unsafe { board.on_unchecked(move_.to()).pt() },
            };

            let mut score = captured_value(captured) + stats.cht.get(board, move_).0 as i32;

            if move_.is_promotion() {
                score += captured_value(unsafe { move_.promotion_pt() });
            }

            self.scores[i] = score;

            if see(board, move_, Eval::ZERO) {
                self.scores.swap(i, next_good_cap);
                self.move_list.swap(i, next_good_cap);
                next_good_cap += 1;
            }
        }

        self.bad_cap_start = next_good_cap;
    }

    fn score_quiets(
        &mut self,
        board: &Board,
        stats: &SearchStats,
        ss_buffer: &[SearchStackEntry; CONT_HIST_SIZE],
    ) {
        for i in self.quiet_start..self.move_list.len() {
            let move_ = self.move_list[i];

            self.scores[i] = stats.ht.get(board, move_).0 as i32;

            for i in 0..CONT_HIST_SIZE {
                let (piece, to) = ss_buffer[i].piece_to();
                self.scores[i] += stats.ct.get(piece, to).get(board, move_).0 as i32;
            }
        }
    }

    fn gen_quiets(
        &mut self,
        board: &Board,
        stats: &SearchStats,
        ss_buffer: &[SearchStackEntry; CONT_HIST_SIZE],
    ) {
        board.generate_moves::<QuietGen>(&mut self.move_list);

        self.score_quiets(board, stats, ss_buffer);
        self.partial_sort(self.quiet_start, self.move_list.len());
    }

    fn gen_captures(&mut self, board: &Board, stats: &SearchStats) {
        board.generate_moves::<CaptureGen>(&mut self.move_list);
        self.quiet_start = self.move_list.len();
        self.bad_cap_start = 0;

        self.score_captures(board, stats);
        self.partial_sort(0, self.bad_cap_start);
        self.partial_sort(self.bad_cap_start, self.move_list.len());
    }

    fn partial_sort(&mut self, start: usize, end: usize) {
        for i in start..end {
            let mut j = i;
            let move_ = self.move_list[j];
            let score = self.scores[j];

            while j > start && self.scores[j - 1] < self.scores[j] {
                self.move_list.swap(j, j - 1);
                self.scores.swap(j, j - 1);
                j -= 1;
            }

            self.scores[j] = score;
            self.move_list[j] = move_;
        }
    }

    fn next_best<F>(&mut self, end: usize, pred: F) -> Option<Move>
    where
        F: Fn(Move) -> bool,
    {
        let slice_len = end.saturating_sub(self.index); // How many items to potentially look at

        let found = self
            .move_list
            .iter()
            .skip(self.index)
            .take(slice_len)
            .enumerate()
            .find(|&(_i, &m)| pred(m) && m != self.tt_move); // Find the first suitable move

        // Update the main index based on whether a move was found
        self.index = found.map_or(end, |(i, _m)| self.index + i + 1);

        found.map(|(_, m)| *m) // Return the move itself if found
    }

    pub(crate) fn next(
        &mut self,
        board: &Board,
        stats: &SearchStats,
        ss_buffer: &[SearchStackEntry; CONT_HIST_SIZE],
    ) -> Option<Move> {
        let killers = self.killers;
        let counter = self.counter;
        let cap_pred = |_| true;
        let quiet_pred =
            |move_: Move| move_ != killers[0] && move_ != killers[1] && move_ != counter;

        match self.stage {
            MoveStage::TTMove => {
                self.stage = MoveStage::GenCaptures;
                if self.tt_move.is_valid() {
                    Some(self.tt_move)
                } else {
                    self.next(board, stats, ss_buffer)
                }
            }
            MoveStage::GenCaptures => {
                self.stage = MoveStage::GoodCaptures;
                self.gen_captures(board, stats);
                self.next(board, stats, ss_buffer)
            }
            MoveStage::GoodCaptures => {
                if let Some(move_) = self.next_best(self.bad_cap_start, cap_pred) {
                    Some(move_)
                } else {
                    self.stage = if self.skip_quiets {
                        self.index = self.bad_cap_start;
                        MoveStage::BadCaptures
                    } else {
                        MoveStage::CounterMove
                    };
                    self.next(board, stats, ss_buffer)
                }
            }
            MoveStage::CounterMove => {
                self.stage = MoveStage::Killer1;

                if !self.skip_quiets
                    && self.counter.is_valid()
                    && self.counter != self.killers[0]
                    && self.counter != self.killers[1]
                    && self.counter != self.tt_move
                {
                    Some(self.counter)
                } else {
                    self.next(board, stats, ss_buffer)
                }
            }
            MoveStage::Killer1 => {
                self.stage = MoveStage::Killer2;
                if !self.skip_quiets
                    && self.killers[0].is_valid()
                    && self.killers[0] != self.tt_move
                {
                    Some(self.killers[0])
                } else {
                    self.next(board, stats, ss_buffer)
                }
            }
            MoveStage::Killer2 => {
                self.stage = MoveStage::GenQuiets;
                if !self.skip_quiets
                    && self.killers[1].is_valid()
                    && self.killers[1] != self.tt_move
                {
                    Some(self.killers[1])
                } else {
                    self.next(board, stats, ss_buffer)
                }
            }
            MoveStage::GenQuiets => {
                if !self.skip_quiets {
                    self.gen_quiets(board, stats, ss_buffer);

                    self.stage = MoveStage::Quiets;
                    self.index = self.quiet_start;
                } else {
                    self.stage = MoveStage::BadCaptures;
                    self.index = self.bad_cap_start;
                }
                self.next(board, stats, ss_buffer)
            }
            MoveStage::Quiets => {
                let next_best = self.next_best(self.move_list.len(), quiet_pred);
                if !self.skip_quiets && next_best.is_some() {
                    next_best
                } else {
                    self.stage = MoveStage::BadCaptures;
                    self.index = self.bad_cap_start;
                    self.next(board, stats, ss_buffer)
                }
            }
            MoveStage::BadCaptures => {
                if let Some(move_) = self.next_best(self.quiet_start, cap_pred) {
                    Some(move_)
                } else {
                    None
                }
            }
        }
    }
}
