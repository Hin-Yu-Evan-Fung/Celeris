pub mod fen;
pub mod mask;
pub mod movegen;
pub mod movement;
pub mod zobrist;

pub use fen::{KILLER_FEN, START_FEN, TRICKY_FEN};
pub use movegen::{
    CaptureGen, LegalGen, MoveList, QuietGen, attacks, bishop_attacks, king_attack, knight_attack,
    pawn_attack, queen_attacks, rook_attacks, sq_dist,
};
pub use zobrist::KeyBundle;

use crate::core::*;
use mask::CastlingMask;

/******************************************\
|==========================================|
|                Constants                 |
|==========================================|
\******************************************/

pub const MAX_MOVES: usize = 256;

/******************************************\
|==========================================|
|               Board State                |
|==========================================|
\******************************************/

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct BoardState {
    pub repetitions: i8,

    fifty_move: u8,

    captured: Option<Piece>,

    enpassant: Option<Square>,

    castle: Castling,

    keys: KeyBundle,

    check_mask: Bitboard,

    diag_pin: Bitboard,

    hv_pin: Bitboard,

    attacked: Bitboard,

    ep_pin: bool,
}

impl BoardState {
    pub(super) fn snapshot(&self) -> Self {
        Self {
            fifty_move: self.fifty_move,
            castle: self.castle,
            keys: self.keys,
            enpassant: self.enpassant,
            ..Default::default()
        }
    }
}

/******************************************\
|==========================================|
|                  Board                   |
|==========================================|
\******************************************/

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Board {
    board: [Option<Piece>; Square::NUM],

    pub pieces: [Bitboard; PieceType::NUM],

    pub occupied: [Bitboard; Colour::NUM],

    castling_mask: CastlingMask,

    half_moves: u16,

    stm: Colour,

    chess960: bool,

    pub state: BoardState,

    history: Vec<BoardState>,
}

/******************************************\
|==========================================|
|           Basic Implementation           |
|==========================================|
\******************************************/

impl Default for Board {
    fn default() -> Board {
        let mut board = Board::new();
        board.set(START_FEN).unwrap();
        board
    }
}

impl Board {
    pub(crate) fn new() -> Board {
        Board {
            board: [None; Square::NUM],
            pieces: [Bitboard::EMPTY; PieceType::NUM],
            occupied: [Bitboard::EMPTY; Colour::NUM],
            castling_mask: CastlingMask::default(),
            stm: Colour::White,
            half_moves: 0,
            state: BoardState::default(),
            history: Vec::with_capacity(MAX_MOVES),
            chess960: false,
        }
    }

    pub fn set_chess960(&mut self, chess960: bool) {
        self.chess960 = chess960;
    }

    pub fn chess960(&self) -> bool {
        self.chess960
    }

    #[inline]
    pub fn on(&self, square: Square) -> Option<Piece> {
        unsafe { *self.board.get_unchecked(square.index()) }
    }

    #[inline]
    pub unsafe fn on_unchecked(&self, square: Square) -> Piece {
        unsafe { self.board[square.index()].unwrap_unchecked() }
    }

    #[inline]
    pub fn piecetype_bb(&self, piecetype: PieceType) -> Bitboard {
        unsafe { *self.pieces.get_unchecked(piecetype.index()) }
    }

    #[inline]
    pub fn occupied_bb(&self, colour: Colour) -> Bitboard {
        unsafe { *self.occupied.get_unchecked(colour.index()) }
    }

    #[inline]
    pub fn all_occupied_bb(&self) -> Bitboard {
        self.occupied_bb(Colour::White) | self.occupied_bb(Colour::Black)
    }

    #[inline]
    pub fn piece_bb(&self, col: Colour, pt: PieceType) -> Bitboard {
        self.piecetype_bb(pt) & self.occupied_bb(col)
    }

    #[inline]
    pub fn stm(&self) -> Colour {
        self.stm
    }

    #[inline]
    pub fn half_moves(&self) -> u16 {
        self.half_moves
    }

    #[inline]
    pub(crate) fn castling_rights(&self, square: Square) -> Castling {
        unsafe { *self.castling_mask.castling.get_unchecked(square.index()) }
    }

    #[inline]
    pub fn ep(&self) -> Option<Square> {
        self.state.enpassant
    }

    #[inline]
    pub fn ep_target(&self) -> Option<Square> {
        self.state
            .enpassant
            .map(|sq| unsafe { sq.add_unchecked(-self.stm.forward()) })
    }

    #[inline]
    pub(crate) fn rook_sq(&self, rights: Castling) -> Square {
        unsafe {
            self.castling_mask
                .rook_sq
                .get_unchecked(rights.0.trailing_zeros() as usize)
                .unwrap_unchecked()
        }
    }

    #[inline]
    pub fn castling(&self) -> Castling {
        self.state.castle
    }

    #[inline]
    pub fn castling_rights_for_side(&self, side: Colour) -> Castling {
        self.state.castle
            & match side {
                Colour::White => Castling::WHITE_CASTLING,
                Colour::Black => Castling::BLACK_CASTLING,
            }
    }

    #[inline]
    pub fn key(&self) -> u64 {
        self.state.keys.key
    }

    #[inline]
    pub fn pawn_key(&self) -> u64 {
        self.state.keys.pawn_key
    }

    #[inline]
    pub fn non_pawn_keys(&self) -> [u64; Colour::NUM] {
        self.state.keys.non_pawn_key
    }

    #[inline]
    pub fn non_pawn_key(&self, col: Colour) -> u64 {
        self.state.keys.non_pawn_key[col.index()]
    }

    #[inline]
    pub fn in_check(&self) -> bool {
        self.state.check_mask != Bitboard::FULL
    }

    #[inline]
    pub fn is_draw(&self, ply: u16) -> bool {
        let move_list = MoveList::new();

        if self.state.fifty_move > 99 && (!self.in_check() || move_list.len() == 0) {
            return true;
        }

        return self.state.repetitions != 0 && self.state.repetitions < (ply as i8);
    }

    #[inline]
    pub fn is_semi_open_file(&self, col: Colour, sq: Square) -> bool {
        (sq.file().bb() & self.piece_bb(col, PieceType::Pawn)).is_empty()
    }

    #[inline]
    pub fn is_open_file(&self, sq: Square) -> bool {
        (sq.file().bb() & self.piecetype_bb(PieceType::Pawn)).is_empty()
    }

    pub fn attackers_to(&self, to: Square, occ: Bitboard) -> Bitboard {
        use crate::core::{Colour::*, PieceType::*};
        pawn_attack(White, to) & self.piece_bb(Black, Pawn)
            | pawn_attack(Black, to) & self.piece_bb(White, Pawn)
            | knight_attack(to) & self.piecetype_bb(Knight)
            | bishop_attacks(to, occ) & (self.piecetype_bb(Bishop) | self.piecetype_bb(Queen))
            | rook_attacks(to, occ) & (self.piecetype_bb(Rook) | self.piecetype_bb(Queen))
            | king_attack(to) & self.piecetype_bb(King)
    }

    pub fn has_non_pawn_material(&self, col: Colour) -> bool {
        (self.occupied_bb(col)
            & !(self.piecetype_bb(PieceType::Pawn) | self.piecetype_bb(PieceType::King)))
        .is_occupied()
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const SEPARATOR: &str = "\n     +---+---+---+---+---+---+---+---+";

        writeln!(f, "{}", SEPARATOR)?;

        for rank in Rank::iter().rev() {
            write!(f, " {}   |", rank as u8 + 1)?;

            for file in File::iter() {
                let square = Square::from_parts(file, rank);
                let cell = match self.on(square) {
                    Some(piece) => piece.to_string(),
                    None => " ".to_string(),
                };
                write!(f, " {} |", cell)?;
            }

            writeln!(f, "{}", SEPARATOR)?;
        }

        writeln!(f)?;
        writeln!(f, "       A   B   C   D   E   F   G   H")?;
        writeln!(f)?;
        writeln!(f, "Current Side: {:?}", self.stm())?;
        writeln!(f, "Castling: {}", self.state.castle)?;
        writeln!(
            f,
            "En Passant Square: {}",
            match self.state.enpassant {
                Some(square) => square.to_string(),
                None => "None".to_string(),
            }
        )?;
        writeln!(f, "Half Move Clock: {}", self.state.fifty_move)?;
        writeln!(f, "Full Move: {}", self.half_moves / 2 + 1)?;
        writeln!(f, "Fen: {}", self.fen())?;
        writeln!(f, "Key: {:#X}", self.state.keys.key)?;
        writeln!(f, "Pawn Key: {:#X}", self.state.keys.pawn_key)?;
        writeln!(
            f,
            "White Non Pawn Key: {:#X}",
            self.state.keys.non_pawn_key[Colour::White.index()]
        )?;
        writeln!(
            f,
            "Black Non Pawn Key: {:#X}",
            self.state.keys.non_pawn_key[Colour::Black.index()]
        )?;

        Ok(())
    }
}
