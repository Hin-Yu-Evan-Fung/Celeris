use std::str::{FromStr, SplitWhitespace};

use super::TimeControl;
use chess::{
    Move, MoveFlag,
    board::{Board, LegalGen, MoveList},
};

use super::options::EngineOption;

#[derive(Debug, PartialEq, Eq)]

pub enum Command {
    Uci,
    Debug(bool),
    IsReady,
    SetOption(EngineOption),
    NewGame,
    Position(Board),
    Go(TimeControl),
    Stop,
    Quit,

    Perft(usize),
    Print,
    Bench,
    Eval,
}

impl FromStr for Command {
    type Err = UCICommandError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();

        match tokens.next() {
            Some("uci") => Ok(Self::Uci),
            Some("debug") => Self::parse_debug(tokens),
            Some("isready") => Ok(Self::IsReady),
            Some("setoption") => Self::parse_option(tokens),
            Some("ucinewgame") => Ok(Self::NewGame),
            Some("position") => Self::parse_position(tokens),
            Some("go") => Self::parse_go(tokens),
            Some("stop") => Ok(Self::Stop),
            Some("quit") => Ok(Self::Quit),
            Some("perft") => Self::parse_perft(tokens),

            Some("bench") => Ok(Self::Bench),
            Some("eval") => Ok(Self::Eval),
            Some("b") => Ok(Self::Print),
            Some(_) => Err(UCICommandError(format!("Invalid command"))),
            None => Err(UCICommandError(format!("Empty command"))),
        }
    }
}

impl Command {
    fn parse_debug<'a>(mut tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        match tokens.next() {
            Some("on") => Ok(Self::Debug(true)),
            Some("off") => Ok(Self::Debug(false)),
            _ => Err(UCICommandError(format!("Invalid debug command"))),
        }
    }

    fn parse_option<'a>(mut tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        if tokens.next() != Some("name") {
            return Err(UCICommandError(
                "Expected 'name' after setoption".to_string(),
            ));
        }

        let mut name_parts = Vec::new();
        let mut next_token = tokens.next();
        while let Some(token) = next_token {
            if token == "value" {
                break;
            }
            name_parts.push(token);
            next_token = tokens.next();
        }

        let option_name = name_parts.join(" ");
        if option_name.is_empty() {
            return Err(UCICommandError("Missing option name".to_string()));
        }

        let option = EngineOption::try_from((option_name, tokens))?;

        Ok(Command::SetOption(option))
    }

    fn parse_position<'a>(mut tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        let mut board = match tokens.next() {
            Some("startpos") => Board::default(),
            Some("fen") => {
                let fen_parts: Vec<&str> = tokens.by_ref().take(Board::FEN_SECTIONS).collect();

                Self::parse_fen(fen_parts)?
            }
            _ => return Err(UCICommandError(format!("Invalid position command"))),
        };

        match tokens.next() {
            Some("moves") => {
                for move_str in tokens {
                    let move_ = Self::parse_move(move_str, &board)?;
                    board.make_move(move_);
                }
            }

            _ => {}
        };

        Ok(Command::Position(board))
    }

    fn parse_fen<'a>(fen_parts: Vec<&str>) -> Result<Board, UCICommandError> {
        if fen_parts.len() < 6 {
            return Err(UCICommandError(
                "Incomplete FEN string provided. Expected 6 fields.".to_string(),
            ));
        }

        let fen_str = fen_parts.join(" ");
        Ok(Board::from_fen(&fen_str)
            .map_err(|e| UCICommandError(format!("Invalid FEN string -> {}", e)))?)
    }

    fn parse_move<'a>(move_str: &str, board: &Board) -> Result<Move, UCICommandError> {
        let mut move_list = MoveList::new();
        board.generate_moves::<LegalGen>(&mut move_list);

        let move_match = |move_: &&Move| {
            move_.to_str(&board) == move_str
                || (move_.flag() == MoveFlag::KingCastle && move_str == "O-O")
                || (move_.flag() == MoveFlag::QueenCastle && move_str == "O-O-O")
        };

        move_list
            .iter()
            .find(move_match)
            .ok_or_else(|| UCICommandError(format!("Invalid move: {}", move_str)))
            .copied()
    }

    fn parse_go<'a>(tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        let tc = tokens
            .collect::<Vec<&str>>()
            .join(" ")
            .trim()
            .parse::<TimeControl>()
            .map_err(|e| UCICommandError(format!("Invalid go command -> {e}")))?;
        Ok(Self::Go(tc))
    }

    fn parse_perft<'a>(mut tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        match tokens.next() {
            Some(depth) => {
                let depth_usize = depth
                    .parse::<usize>()
                    .map_err(|e| UCICommandError(format!("Invalid perft depth -> {}", e)))?;
                Ok(Self::Perft(depth_usize))
            }
            _ => Err(UCICommandError(format!("Invalid perft command"))),
        }
    }
}

#[derive(Debug)]
pub struct UCICommandError(pub String);

impl std::fmt::Display for UCICommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UCI Command Error -> {}", self.0)
    }
}

impl std::error::Error for UCICommandError {}
