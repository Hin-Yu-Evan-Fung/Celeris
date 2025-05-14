use super::Board;

use super::movegen::pin_bb;
use crate::core::*;

/******************************************\
|==========================================|
|            Useful fen strings            |
|==========================================|
\******************************************/

pub const EMPTY_FEN: &str = "r2k3r/8/8/8/8/8/8/R2K3R w KQkq - 0 1";

pub const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

pub const TRICKY_FEN: &str = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";

pub const KILLER_FEN: &str = "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1";

/******************************************\
|==========================================|
|               Parse Fen                  |
|==========================================|
\******************************************/

impl Board {
    pub const FEN_SECTIONS: usize = 6;

    pub fn set(&mut self, fen: &str) -> Result<(), FenParseError> {
        *self = Board::new();

        let mut parts = fen.split_whitespace();

        let piece_placement = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        self.parse_piece_placement(piece_placement)?;

        let side_to_move = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        self.parse_side_to_move(side_to_move)?;

        let castling = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        self.parse_castling(castling)?;

        let enpassant = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;
        self.parse_enpassant(enpassant)?;

        let fifty_move_token = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;

        self.state.fifty_move = self.parse_fifty_move(fifty_move_token)?;

        let full_move_token = parts.next().ok_or(FenParseError::InvalidNumberOfFields)?;

        self.half_moves = self.parse_full_move(full_move_token)?;

        if parts.next().is_some() {
            return Err(FenParseError::InvalidNumberOfFields);
        }

        self.state.keys.key = self.calc_key();
        self.state.keys.pawn_key = self.calc_pawn_key();
        self.state.keys.non_pawn_key = self.calc_non_pawn_key();

        self.update_masks();

        Ok(())
    }

    pub fn from_fen(fen: &str) -> Result<Self, FenParseError> {
        let mut board = Board::new();
        board.set(fen)?;
        Ok(board)
    }

    pub fn fen(&self) -> String {
        let mut fen = String::new();

        for rank in Rank::iter().rev() {
            let mut empty_count = 0;
            for file in File::iter() {
                let square = Square::from_parts(file, rank);
                match self.on(square) {
                    Some(piece) => {
                        if empty_count > 0 {
                            fen.push_str(&empty_count.to_string());
                            empty_count = 0;
                        }
                        fen.push_str(&piece.to_string());
                    }
                    None => {
                        empty_count += 1;
                    }
                }
            }
            if empty_count > 0 {
                fen.push_str(&empty_count.to_string());
            }
            if rank != Rank::Rank1 {
                fen.push('/');
            }
        }

        fen.push(' ');
        fen.push_str(match self.stm {
            Colour::White => "w",
            Colour::Black => "b",
        });

        let white_ksq = self.piece_bb(Colour::White, PieceType::King).lsb().unwrap();
        let black_ksq = self.piece_bb(Colour::Black, PieceType::King).lsb().unwrap();

        fen.push(' ');
        if self.state.castle == Castling::NONE {
            fen.push('-');
        } else {
            if self.state.castle.has(Castling::WK) {
                let wk_side_rooks =
                    self.piece_bb(Colour::White, PieceType::Rook) & pin_bb(white_ksq, Square::H1);
                if wk_side_rooks.count_bits() == 1 {
                    fen.push('K');
                } else {
                    let rook_sq = self.castling_mask.rook_sq[0]
                        .expect(" There should be a rook square set in Chess960 Positions");
                    fen.push(
                        rook_sq
                            .file()
                            .to_string()
                            .to_uppercase()
                            .chars()
                            .next()
                            .unwrap(),
                    );
                }
            }
            if self.state.castle.has(Castling::WQ) {
                let wq_side_rooks =
                    self.piece_bb(Colour::White, PieceType::Rook) & pin_bb(white_ksq, Square::A1);
                if wq_side_rooks.count_bits() == 1 {
                    fen.push('Q');
                } else {
                    let rook_sq = self.castling_mask.rook_sq[1]
                        .expect(" There should be a rook square set in Chess960 Positions");
                    fen.push(
                        rook_sq
                            .file()
                            .to_string()
                            .to_uppercase()
                            .chars()
                            .next()
                            .unwrap(),
                    );
                }
            }
            if self.state.castle.has(Castling::BK) {
                let bk_side_rooks =
                    self.piece_bb(Colour::Black, PieceType::Rook) & pin_bb(black_ksq, Square::H8);
                if bk_side_rooks.count_bits() == 1 {
                    fen.push('k');
                } else {
                    let rook_sq = self.castling_mask.rook_sq[2]
                        .expect(" There should be a rook square set in Chess960 Positions");
                    fen.push(rook_sq.file().to_string().chars().next().unwrap());
                }
            }
            if self.state.castle.has(Castling::BQ) {
                let bq_side_rooks =
                    self.piece_bb(Colour::Black, PieceType::Rook) & pin_bb(black_ksq, Square::A8);
                if bq_side_rooks.count_bits() == 1 {
                    fen.push('q');
                } else {
                    let rook_sq = self.castling_mask.rook_sq[3]
                        .expect(" There should be a rook square set in Chess960 Positions");
                    fen.push(rook_sq.file().to_string().chars().next().unwrap());
                }
            }
        }

        fen.push(' ');
        match self.state.enpassant {
            Some(square) => fen.push_str(&square.to_string()),
            None => fen.push('-'),
        }

        fen.push_str(&format!(" {}", self.state.fifty_move));

        fen.push_str(&format!(" {}", (self.half_moves / 2) + 1));

        fen
    }

    fn parse_separator(
        rank_iter: &mut impl DoubleEndedIterator<Item = Rank>,
        rank: Rank,
        file: u8,
    ) -> Result<(Rank, u8), FenParseError> {
        if file != 8 {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Rank {:?} ended prematurely at file index {} (expected 8) before '/'",
                rank, file
            )));
        }

        let next_rank = rank_iter.next().ok_or_else(|| {
            FenParseError::InvalidRankFormat(format!(
                "Too many rank separators ('/') found after completing rank {:?}",
                rank
            ))
        })?;

        Ok((next_rank, 0))
    }

    fn parse_skip(
        skip: char,
        idx: usize,
        current_rank: Rank,
        current_file_index: u8,
    ) -> Result<u8, FenParseError> {
        let skip_val = skip.to_digit(10).unwrap();

        if !(1..=8).contains(&skip_val) {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Invalid skip digit '{}' (must be 1-8) at char index {}",
                skip, idx
            )));
        }

        let skip_u8 = skip_val as u8;

        if current_file_index + skip_u8 > 8 {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Skip value {} exceeds rank length at file index {} on rank {:?}",
                skip_u8, current_file_index, current_rank
            )));
        }

        Ok(skip_u8)
    }

    fn parse_piece(&mut self, piece: char, rank: Rank, file: u8) -> Result<(), FenParseError> {
        if file >= 8 {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Piece placement '{}' attempted beyond file H (index >= 8) on rank {:?}",
                piece, rank
            )));
        }

        let piece_enum = piece
            .to_string()
            .parse::<Piece>()
            .map_err(|_| FenParseError::InvalidPiecePlacementChar(piece))?;

        let current_file = File::from_unchecked(file);

        let sq = Square::from_parts(current_file, rank);

        self.add_piece(piece_enum, sq);

        Ok(())
    }

    fn parse_piece_placement(&mut self, piece_placement: &str) -> Result<(), FenParseError> {
        let mut rank_iter = Rank::iter().rev();

        let mut rank = rank_iter
            .next()
            .ok_or_else(|| FenParseError::InvalidRankFormat("Board has no ranks?".to_string()))?;

        let mut file: u8 = 0;

        for (i, char) in piece_placement.chars().enumerate() {
            match char {
                '/' => {
                    (rank, file) = Self::parse_separator(&mut rank_iter, rank, file)?;
                }

                skip if skip.is_ascii_digit() => {
                    file += Self::parse_skip(skip, i, rank, file)?;
                }

                piece_char => {
                    self.parse_piece(piece_char, rank, file)?;
                    file += 1;
                }
            }
        }

        if file != 8 {
            return Err(FenParseError::InvalidRankFormat(format!(
                "Final rank {:?} ended prematurely at file index {} (expected 8)",
                rank, file
            )));
        }

        if rank_iter.next().is_some() {
            return Err(FenParseError::InvalidRankFormat(
                "Not enough ranks specified in FEN string (expected 8)".to_string(),
            ));
        }

        Ok(())
    }

    fn parse_side_to_move(&mut self, side_to_move: &str) -> Result<(), FenParseError> {
        match side_to_move {
            "w" => self.stm = Colour::White,
            "b" => self.stm = Colour::Black,
            _ => return Err(FenParseError::InvalidSideToMove(side_to_move.to_string())),
        };
        Ok(())
    }

    fn parse_castling(&mut self, castling: &str) -> Result<(), FenParseError> {
        self.state.castle = Castling::NONE;

        let white_ksq = self
            .piece_bb(Colour::White, PieceType::King)
            .pop_lsb()
            .expect("There should be exactly one white king on the board");
        let black_ksq = self
            .piece_bb(Colour::Black, PieceType::King)
            .pop_lsb()
            .expect("There should be exactly one black king on the board");

        self.castling_mask.castling[white_ksq.index()].remove(Castling::WHITE_CASTLING);
        self.castling_mask.castling[black_ksq.index()].remove(Castling::BLACK_CASTLING);

        if castling == "-" {
            return Ok(());
        }

        for c in castling.chars() {
            match c {
                'K' => {
                    self.state.castle.set(Castling::WK);
                    let wk_side_rook = self.piece_bb(Colour::White, PieceType::Rook)
                        & pin_bb(white_ksq, Square::H1);
                    let rook_sq = wk_side_rook
                        .lsb()
                        .expect("There should be exactly one king side white rook on the board when the castling flag K is set");
                    self.castling_mask.rook_sq[0] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(Castling::WK);
                }
                'Q' => {
                    self.state.castle.set(Castling::WQ);
                    let wq_side_rook = self.piece_bb(Colour::White, PieceType::Rook)
                        & pin_bb(white_ksq, Square::A1);
                    let rook_sq = wq_side_rook
                    .lsb()
                    .expect("There should be exactly one queen side white rook on the board when the castling flag Q is set");
                    self.castling_mask.rook_sq[1] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(Castling::WQ);
                }
                'k' => {
                    self.state.castle.set(Castling::BK);
                    let bk_side_rook = self.piece_bb(Colour::Black, PieceType::Rook)
                        & pin_bb(black_ksq, Square::H8);

                    let rook_sq = bk_side_rook
                    .lsb()
                    .expect("There should be exactly one king side black rook on the board when the castling flag k is set");
                    self.castling_mask.rook_sq[2] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(Castling::BK);
                }
                'q' => {
                    self.state.castle.set(Castling::BQ);

                    let bq_side_rook = self.piece_bb(Colour::Black, PieceType::Rook)
                        & pin_bb(black_ksq, Square::A8);

                    let rook_sq = bq_side_rook
                    .lsb()
                    .expect("There should be exactly one queen side black rook on the board when the castling flag q is set");
                    self.castling_mask.rook_sq[3] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(Castling::BQ);
                }
                'A'..='H' | 'a'..='h' => {
                    let is_white = c.is_uppercase();
                    let ksq = if is_white { white_ksq } else { black_ksq };
                    let back_rank = if is_white { Rank::Rank1 } else { Rank::Rank8 };
                    let file = c.to_lowercase().to_string().parse::<File>().unwrap();
                    let is_king_side = file >= ksq.file();

                    let rook_sq = Square::from_parts(file, back_rank);

                    assert!(
                        self.on(rook_sq).map(|pc| pc.pt()) == Some(PieceType::Rook),
                        "{rook_sq}"
                    );

                    let index = 2 * !is_white as usize + !is_king_side as usize;

                    let rights = [Castling::WK, Castling::WQ, Castling::BK, Castling::BQ][index];

                    self.state.castle.set(rights);
                    self.castling_mask.rook_sq[index] = Some(rook_sq);
                    self.castling_mask.castling[rook_sq.index()].remove(rights);
                }

                _ => return Err(FenParseError::InvalidCastlingChar(c)),
            };
        }

        Ok(())
    }

    fn parse_enpassant(&mut self, enpassant: &str) -> Result<(), FenParseError> {
        self.state.enpassant = match enpassant {
            "-" => None,

            _ => {
                let square = enpassant
                    .parse::<Square>()
                    .map_err(|_| FenParseError::InvalidEnPassantSquare(enpassant.to_string()))?;

                if ![Rank::Rank3, Rank::Rank6].contains(&square.rank()) {
                    return Err(FenParseError::InvalidEnPassantSquare(format!(
                        "{square} is not a valid enpassant square"
                    )));
                }
                Some(square)
            }
        };
        Ok(())
    }

    fn parse_fifty_move(&mut self, fifty_move_token: &str) -> Result<u8, FenParseError> {
        fifty_move_token
            .parse::<u8>()
            .map_err(|_| FenParseError::InvalidHalfmoveClock(fifty_move_token.to_string()))
    }

    fn parse_full_move(&mut self, full_move_token: &str) -> Result<u16, FenParseError> {
        let full_move_number = full_move_token
            .parse::<u16>()
            .map_err(|_| FenParseError::InvalidFullmoveNumber(full_move_token.to_string()))?;

        if full_move_number == 0 {
            return Err(FenParseError::InvalidFullmoveNumber(format!(
                "Fullmove number cannot be 0, found: {}",
                full_move_token
            )));
        }

        let ply = (full_move_number - 1) * 2 + (self.stm() as u16);

        Ok(ply)
    }
}

/******************************************\
|==========================================|
|             Fen Parse Errors             |
|==========================================|
\******************************************/

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FenParseError {
    InvalidNumberOfFields,

    InvalidPiecePlacementChar(char),

    InvalidRankFormat(String),

    InvalidSideToMove(String),

    InvalidCastlingChar(char),

    InvalidEnPassantSquare(String),

    InvalidHalfmoveClock(String),

    InvalidFullmoveNumber(String),
}

impl std::fmt::Display for FenParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FenParseError::InvalidNumberOfFields => {
                write!(f, "FEN string must have 6 fields separated by spaces")
            }
            FenParseError::InvalidPiecePlacementChar(c) => {
                write!(f, "Invalid character in FEN piece placement: '{}'", c)
            }
            FenParseError::InvalidRankFormat(reason) => {
                write!(f, "Invalid rank format in FEN piece placement: {}", reason)
            }
            FenParseError::InvalidSideToMove(s) => {
                write!(
                    f,
                    "Invalid side to move in FEN: '{}', expected 'w' or 'b'",
                    s
                )
            }
            FenParseError::InvalidCastlingChar(c) => {
                write!(f, "Invalid character in FEN castling availability: '{}'", c)
            }
            FenParseError::InvalidEnPassantSquare(s) => {
                write!(f, "Invalid en passant target square in FEN: '{}'", s)
            }
            FenParseError::InvalidHalfmoveClock(s) => {
                write!(f, "Invalid halfmove clock value in FEN: '{}'", s)
            }
            FenParseError::InvalidFullmoveNumber(s) => {
                write!(f, "Invalid fullmove number value in FEN: '{}'", s)
            }
        }
    }
}

impl std::error::Error for FenParseError {}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse_start_fen() {
        let mut board = Board::new();
        assert!(board.set(START_FEN).is_ok());

        assert_eq!(board.on(Square::A1), Some(Piece::WhiteRook));
        assert_eq!(board.on(Square::E1), Some(Piece::WhiteKing));
        assert_eq!(board.on(Square::H8), Some(Piece::BlackRook));
        assert_eq!(board.on(Square::D8), Some(Piece::BlackQueen));
        assert_eq!(board.on(Square::E4), None);
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.state.castle, Castling::ALL);
        assert_eq!(board.state.enpassant, None);
        assert_eq!(board.state.fifty_move, 0);
        assert_eq!(board.half_moves(), 0);
        assert_eq!(board.fen(), START_FEN.trim());
    }

    #[test]
    fn test_parse_empty_fen() {
        let mut board = Board::new();
        assert!(board.set(EMPTY_FEN).is_ok());

        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.state.castle, Castling::ALL);
        assert_eq!(board.state.enpassant, None);
        assert_eq!(board.state.fifty_move, 0);
        assert_eq!(board.half_moves(), 0);
        assert_eq!(board.fen(), EMPTY_FEN.trim());
    }

    #[test]
    fn test_parse_tricky_fen() {
        let mut board = Board::new();

        assert!(board.set(TRICKY_FEN).is_ok());

        assert_eq!(board.on(Square::A8), Some(Piece::BlackRook));
        assert_eq!(board.on(Square::E8), Some(Piece::BlackKing));
        assert_eq!(board.on(Square::H8), Some(Piece::BlackRook));
        assert_eq!(board.on(Square::F3), Some(Piece::WhiteQueen));
        assert_eq!(board.on(Square::C3), Some(Piece::WhiteKnight));
        assert_eq!(board.on(Square::H3), Some(Piece::BlackPawn));
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.state.castle, Castling::ALL);
        assert_eq!(board.state.enpassant, None);
        assert_eq!(board.state.fifty_move, 0);
        assert_eq!(board.half_moves(), 0);
        assert_eq!(board.fen(), TRICKY_FEN.trim());
    }

    #[test]
    fn test_fen_invalid_piece() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/ppppxppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidPiecePlacementChar('x'))
        ));
    }

    #[test]
    fn test_fen_invalid_rank_length_short() {
        let mut board = Board::new();

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));

        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("ended prematurely at file index 7")
        );
    }

    #[test]
    fn test_fen_invalid_rank_length_short_at_end() {
        let mut board = Board::new();

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBN w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Final rank Rank1 ended prematurely at file index 7")
        );
    }

    #[test]
    fn test_fen_invalid_rank_length_long_piece() {
        let mut board = Board::new();

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("attempted beyond file H")
        );
    }

    #[test]
    fn test_fen_invalid_rank_length_long_skip() {
        let mut board = Board::new();

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/P6P1/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Skip value 1 exceeds rank length")
        );
    }

    #[test]
    fn test_fen_invalid_skip_digit_zero() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppp0ppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Invalid skip digit '0'")
        );
    }

    #[test]
    fn test_fen_invalid_skip_digit_nine() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppp9ppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Invalid skip digit '9'")
        );
    }

    #[test]
    fn test_fen_too_many_ranks() {
        let mut board = Board::new();
        let fen = "8/8/8/8/8/8/8/8/8 w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Too many rank separators")
        );
    }

    #[test]
    fn test_fen_too_few_ranks() {
        let mut board = Board::new();
        let fen = "8/8/8/8/8/8/8 w KQkq - 0 1";
        let result = board.set(fen);
        assert!(matches!(result, Err(FenParseError::InvalidRankFormat(_))));
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Not enough ranks specified")
        );
    }

    #[test]
    fn test_fen_missing_fields() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidNumberOfFields)
        ));
    }

    #[test]
    fn test_fen_extra_fields() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 extra";
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidNumberOfFields)
        ));
    }

    #[test]
    fn test_fen_invalid_side() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1";
        assert!(matches!(board.set(fen), Err(FenParseError::InvalidSideToMove(s)) if s == "x"));
    }

    #[test]
    fn test_fen_invalid_castling() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQXkq - 0 1";
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidCastlingChar('X'))
        ));
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w K-q - 0 1";
        assert!(matches!(
            board.set(fen),
            Err(FenParseError::InvalidCastlingChar('-'))
        ));
    }

    #[test]
    fn test_fen_invalid_enpassant() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e9 0 1";
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidEnPassantSquare(s)) if s == "e9")
        );
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq zz 0 1";
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidEnPassantSquare(s)) if s == "zz")
        );
    }

    #[test]
    fn test_fen_invalid_halfmove() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - fifty 1";
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidHalfmoveClock(s)) if s == "fifty")
        );
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - -1 1";
        assert!(matches!(board.set(fen), Err(FenParseError::InvalidHalfmoveClock(s)) if s == "-1"));
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 256 1";
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidHalfmoveClock(s)) if s == "256")
        );
    }

    #[test]
    fn test_fen_invalid_fullmove() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 zero";
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidFullmoveNumber(s)) if s == "zero")
        );
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0";
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidFullmoveNumber(s)) if s.contains("cannot be 0"))
        );
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 -5";
        assert!(
            matches!(board.set(fen), Err(FenParseError::InvalidFullmoveNumber(s)) if s == "-5")
        );
    }

    #[test]
    fn test_fen_ply_calculation() {
        let mut board = Board::new();

        let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 1);
        assert_eq!(board.stm(), Colour::Black);
        assert_eq!(board.fen(), fen.trim());

        let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 2);
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.fen(), fen.trim());

        let fen = "r1bqkbnr/pp1ppppp/2n5/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 1 10";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 18);
        assert_eq!(board.stm(), Colour::White);
        assert_eq!(board.fen(), fen.trim());

        let fen = "r1bqkbnr/pp1ppppp/2n5/2p5/3PP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 10";
        assert!(board.set(fen).is_ok());
        assert_eq!(board.half_moves(), 19);
        assert_eq!(board.stm(), Colour::Black);
        assert_eq!(board.fen(), fen.trim());
    }
}

#[cfg(test)]
mod xfen_tests {
    use super::*;

    fn assert_fen_parses(fen: &str) -> Board {
        Board::from_fen(fen)
            .unwrap_or_else(|e| panic!("FEN failed to parse: '{}', Error: {}", fen, e))
    }

    fn assert_castling_flags(board: &Board, expected_flags: Castling) {
        assert_eq!(
            board.state.castle, expected_flags,
            "Expected castling flags {:?}, but got {:?}",
            expected_flags, board.state.castle
        );
    }

    fn assert_mask_removes(board: &Board, sq: Square, right: Castling, should_remove: bool) {
        let has_right = board.castling_mask.castling[sq.index()].has(right);
        assert_ne!(
            has_right, should_remove,
            "Mask check failed for {:?} on {:?}: expected removal={}, has_right={}",
            right, sq, should_remove, has_right
        );
    }

    fn assert_rook_sq(board: &Board, index: usize, expected_sq: Option<Square>) {
        let actual_sq = board.castling_mask.rook_sq[index];
        assert_eq!(
            actual_sq, expected_sq,
            "Expected rook_sq[{}] to be {:?}, but got {:?}",
            index, expected_sq, actual_sq
        );
    }

    #[test]
    fn test_xfen_white_castling_dg() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/3RKNR1 w DG - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::WQ | Castling::WK);

        let ksq = Square::E1;
        let d_rook_sq = Square::D1;
        let g_rook_sq = Square::G1;

        assert_mask_removes(&board, ksq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, d_rook_sq, Castling::WQ, true);
        assert_mask_removes(&board, d_rook_sq, Castling::WK, false);
        assert_mask_removes(&board, g_rook_sq, Castling::WK, true);
        assert_mask_removes(&board, g_rook_sq, Castling::WQ, false);

        assert_rook_sq(&board, 0, Some(g_rook_sq));
        assert_rook_sq(&board, 1, Some(d_rook_sq));
        assert_rook_sq(&board, 2, None);
        assert_rook_sq(&board, 3, None);
    }

    #[test]
    fn test_xfen_black_castling_cf() {
        let fen = "2r1krn1/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b cf - 0 1";
        let board = assert_fen_parses(fen);

        assert_castling_flags(&board, Castling::BQ | Castling::BK);

        let ksq = Square::E8;
        let c_rook_sq = Square::C8;
        let f_rook_sq = Square::F8;

        assert_mask_removes(&board, ksq, Castling::BLACK_CASTLING, true);
        assert_mask_removes(&board, c_rook_sq, Castling::BQ, true);
        assert_mask_removes(&board, c_rook_sq, Castling::BK, false);
        assert_mask_removes(&board, f_rook_sq, Castling::BK, true);
        assert_mask_removes(&board, f_rook_sq, Castling::BQ, false);

        assert_rook_sq(&board, 0, None);
        assert_rook_sq(&board, 1, None);
        assert_rook_sq(&board, 2, Some(f_rook_sq));
        assert_rook_sq(&board, 3, Some(c_rook_sq));
    }

    #[test]
    fn test_xfen_mixed_castling_bgcf() {
        let fen = "2r1krn1/pppppppp/8/8/8/8/PPPPPPPP/1R2KNR1 w BGcf - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::ALL);

        let wk_sq = Square::E1;
        let bk_sq = Square::E8;
        let w_b_rook = Square::B1;
        let w_g_rook = Square::G1;
        let b_c_rook = Square::C8;
        let b_f_rook = Square::F8;

        assert_mask_removes(&board, wk_sq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, w_b_rook, Castling::WQ, true);
        assert_mask_removes(&board, w_b_rook, Castling::WK, false);
        assert_mask_removes(&board, w_g_rook, Castling::WK, true);
        assert_mask_removes(&board, w_g_rook, Castling::WQ, false);

        assert_mask_removes(&board, bk_sq, Castling::BLACK_CASTLING, true);
        assert_mask_removes(&board, b_c_rook, Castling::BQ, true);
        assert_mask_removes(&board, b_c_rook, Castling::BK, false);
        assert_mask_removes(&board, b_f_rook, Castling::BK, true);
        assert_mask_removes(&board, b_f_rook, Castling::BQ, false);

        assert_rook_sq(&board, 0, Some(w_g_rook));
        assert_rook_sq(&board, 1, Some(w_b_rook));
        assert_rook_sq(&board, 2, Some(b_f_rook));
        assert_rook_sq(&board, 3, Some(b_c_rook));
    }

    #[test]
    fn test_xfen_king_on_corner_white_ah() {
        let fen_sp4 = "rnbqkb1r/pppppppp/8/8/8/8/PPPPPPPP/1NBQKBRR w H - 0 1";
        let board = assert_fen_parses(fen_sp4);
        assert_castling_flags(&board, Castling::WK);

        let ksq = Square::E1;
        let h_rook_sq = Square::H1;

        assert_mask_removes(&board, ksq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, h_rook_sq, Castling::WK, true);
        assert_mask_removes(&board, h_rook_sq, Castling::WQ, false);

        assert_rook_sq(&board, 0, Some(h_rook_sq));
    }

    #[test]
    fn test_xfen_custom_position() {
        let fen_sp512 = "rn2k1r1/ppp1pp1p/3p2p1/5bn1/P7/2N2B2/1PPPPP2/2BNK1RR w Gkq - 4 11";
        let board = assert_fen_parses(fen_sp512);
        assert_castling_flags(&board, Castling::WK | Castling::BK | Castling::BQ);
        assert_mask_removes(&board, Square::E8, Castling::BLACK_CASTLING, true);
        assert_mask_removes(&board, Square::G8, Castling::BK, true);
        assert_mask_removes(&board, Square::A8, Castling::BQ, true);
        assert_rook_sq(&board, 0, Some(Square::G1));
        assert_rook_sq(&board, 1, None);
        assert_rook_sq(&board, 2, Some(Square::G8));
        assert_rook_sq(&board, 3, Some(Square::A8));
    }

    #[test]
    fn test_xfen_no_castling_dash() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::NONE);

        let wk_sq = Square::E1;
        let bk_sq = Square::E8;
        assert_mask_removes(&board, wk_sq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, bk_sq, Castling::BLACK_CASTLING, true);

        assert_rook_sq(&board, 0, None);
        assert_rook_sq(&board, 1, None);
        assert_rook_sq(&board, 2, None);
        assert_rook_sq(&board, 3, None);
    }

    #[test]
    fn test_xfen_only_one_white_right_h() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w H - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::WK);

        let ksq = Square::E1;
        let h_rook_sq = Square::H1;
        let a_rook_sq = Square::A1;

        assert_mask_removes(&board, ksq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, h_rook_sq, Castling::WK, true);
        assert_mask_removes(&board, h_rook_sq, Castling::WQ, false);

        assert_mask_removes(&board, a_rook_sq, Castling::WQ, false);
        assert_mask_removes(&board, a_rook_sq, Castling::WK, false);

        assert_rook_sq(&board, 0, Some(h_rook_sq));
        assert_rook_sq(&board, 1, None);
    }

    #[test]
    fn test_xfen_invalid_char() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KX - 0 1";
        let result = Board::from_fen(fen);
        assert!(matches!(
            result,
            Err(FenParseError::InvalidCastlingChar('X'))
        ));

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Q1 - 0 1";
        let result = Board::from_fen(fen);
        assert!(matches!(
            result,
            Err(FenParseError::InvalidCastlingChar('1'))
        ));
    }

    #[test]
    fn test_xfen_rook_not_present() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3KBN1 w H - 0 1";

        let result = std::panic::catch_unwind(|| Board::from_fen(fen));
        assert!(
            result.is_err(),
            "Parsing FEN with missing rook for castling right should panic"
        );
    }

    #[test]
    fn test_standard_fen_kqkq() {
        let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
        let board = assert_fen_parses(fen);
        assert_castling_flags(&board, Castling::ALL);

        let wk_sq = Square::E1;
        let bk_sq = Square::E8;
        let w_h_rook = Square::H1;
        let w_a_rook = Square::A1;
        let b_h_rook = Square::H8;
        let b_a_rook = Square::A8;

        assert_mask_removes(&board, wk_sq, Castling::WHITE_CASTLING, true);
        assert_mask_removes(&board, w_a_rook, Castling::WQ, true);
        assert_mask_removes(&board, w_h_rook, Castling::WK, true);

        assert_mask_removes(&board, bk_sq, Castling::BLACK_CASTLING, true);
        assert_mask_removes(&board, b_a_rook, Castling::BQ, true);
        assert_mask_removes(&board, b_h_rook, Castling::BK, true);

        assert_rook_sq(&board, 0, Some(w_h_rook));
        assert_rook_sq(&board, 1, Some(w_a_rook));
        assert_rook_sq(&board, 2, Some(b_h_rook));
        assert_rook_sq(&board, 3, Some(b_a_rook));
    }
}
