use std::str::{FromStr, SplitWhitespace};

use super::TimeControl;
use chess::board::Board;

pub enum UCIOption {
    ClearHash(),
    ResizeHash(usize),
    ResizeThreads(usize),
}

pub enum UCICommand {
    // Standard UCI commands from https://www.shredderchess.com/chess-features/uci-universal-chess-interface.html
    Uci,
    Debug(bool),
    IsReady,
    SetOption(UCIOption),
    UciNewGame,
    Position(Board),
    Go(TimeControl),
    Stop,
    Quit,

    // Custom commands
    Perft(usize),
    Print,
    Bench,
    Eval,
}

impl FromStr for UCICommand {
    type Err = UCICommandError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();

        match tokens.next() {
            Some("uci") => Ok(Self::Uci),
            Some("debug") => Self::parse_debug(tokens),
            Some("isready") => Ok(Self::IsReady),
            Some("setoption") => Self::parse_option(tokens),
            Some("ucinewgame") => Ok(Self::UciNewGame),
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

impl UCICommand {
    fn parse_debug<'a>(mut tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        match tokens.next() {
            Some("on") => Ok(Self::Debug(true)),
            Some("off") => Ok(Self::Debug(false)),
            _ => Err(UCICommandError(format!("Invalid debug command"))),
        }
    }

    fn parse_option<'a>(mut tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        let name = match tokens.next() {
            Some("name") => tokens
                .next()
                .ok_or(UCICommandError(format!("No option command")))?
                .to_owned(),
            _ => return Err(UCICommandError(format!("Invalid option command"))),
        };

        let value = match tokens.next() {
            Some("value") => tokens
                .next()
                .ok_or(UCICommandError(format!("No option command")))?
                .to_owned(),
            _ => return Err(UCICommandError(format!("Invalid option command"))),
        };

        let option = match name.to_ascii_lowercase().as_str() {
            "clear" => UCIOption::ClearHash(),
            "hash" => UCIOption::ResizeHash(Self::parse_value(value)?),
            "threads" => UCIOption::ResizeThreads(Self::parse_value(value)?),
            _ => return Err(UCICommandError(format!("Invalid option name"))),
        };

        Ok(UCICommand::SetOption(option))
    }

    fn parse_value<T: FromStr>(value: String) -> Result<T, UCICommandError> {
        value
            .parse::<T>()
            .map_err(|_| UCICommandError(format!("Invalid value type")))
    }

    fn parse_position<'a>(mut tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        match tokens.next() {
            Some("startpos") => Ok(Self::Position(Board::default())),
            Some("fen") => Self::parse_fen(tokens),
            _ => Err(UCICommandError(format!("Invalid position command"))),
        }
    }

    fn parse_fen<'a>(tokens: SplitWhitespace) -> Result<Self, UCICommandError> {
        let fen = tokens.collect::<Vec<&str>>().join(" ");
        let board = Board::from_fen(&fen);
        match board {
            Ok(board) => Ok(Self::Position(board)),
            Err(e) => Err(UCICommandError(format!("Invalid Fen -> {}", e))),
        }
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
pub struct UCICommandError(String);

impl std::fmt::Display for UCICommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UCI Command Error -> {}", self.0)
    }
}

impl std::error::Error for UCICommandError {}
