use std::str::FromStr;

use chess::board::Board;

pub enum EngineCommand {
    // Standard UCI commands from https://www.shredderchess.com/chess-features/uci-universal-chess-interface.html
    Uci,
    Debug(bool),
    IsReady,
    SetOption(String, String),
    UciNewGame,
    Position(Board),
    // Go(TimeControl),
    Stop,
    Quit,

    // Custom commands
    Perft(usize),
    Print,
    Bench,
    Eval,
}

impl FromStr for EngineCommand {
    type Err = EngineCommandError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();

        match tokens.next() {
            Some("uci") => Ok(Self::Uci),
            Some("debug") => Self::parse_debug(tokens),
            Some("isready") => Ok(Self::IsReady),
            Some("setoption") => Self::parse_option(tokens),
            Some("ucinewgame") => Ok(Self::UciNewGame),
            Some("position") => Self::parse_position(tokens),
            // Some("go") => Self::parse_go(tokens),
            Some("stop") => Ok(Self::Stop),
            Some("quit") => Ok(Self::Quit),
            Some("perft") => Self::parse_perft(tokens),
            Some("bench") => Ok(Self::Bench),
            Some("eval") => Ok(Self::Eval),
            Some("b") => Ok(Self::Print),
            Some(_) => Err(EngineCommandError(format!("Invalid command"))),
            None => Err(EngineCommandError(format!("Empty command"))),
        }
    }
}

impl EngineCommand {
    fn parse_debug<'a>(
        mut tokens: impl Iterator<Item = &'a str>,
    ) -> Result<Self, EngineCommandError> {
        match tokens.next() {
            Some("on") => Ok(Self::Debug(true)),
            Some("off") => Ok(Self::Debug(false)),
            _ => Err(EngineCommandError(format!("Invalid debug command"))),
        }
    }

    fn parse_option<'a>(tokens: impl Iterator<Item = &'a str>) -> Result<Self, EngineCommandError> {
        Ok(Self::SetOption(String::default(), String::default()))
    }

    fn parse_position<'a>(
        mut tokens: impl Iterator<Item = &'a str>,
    ) -> Result<Self, EngineCommandError> {
        match tokens.next() {
            Some("startpos") => Ok(Self::Position(Board::default())),
            Some("fen") => Self::parse_fen(tokens),
            _ => Err(EngineCommandError(format!("Invalid position command"))),
        }
    }

    fn parse_fen<'a>(tokens: impl Iterator<Item = &'a str>) -> Result<Self, EngineCommandError> {
        let fen = tokens.collect::<Vec<&str>>().join(" ");
        let board = Board::from_fen(&fen);
        match board {
            Ok(board) => Ok(Self::Position(board)),
            Err(e) => Err(EngineCommandError(format!("Invalid Fen! Reason: {}", e))),
        }
    }

    // fn parse_go<'a>(tokens: impl Iterator<Item = &'a str>) -> Result<Self, UCICommandError> {
    //     Ok(Self::Go(TimeControl::default()))
    // }

    fn parse_perft<'a>(
        mut tokens: impl Iterator<Item = &'a str>,
    ) -> Result<Self, EngineCommandError> {
        match tokens.next() {
            Some(depth) => {
                let depth_usize = depth.parse::<usize>().map_err(|e| {
                    EngineCommandError(format!("Invalid perft depth! Reason: {}", e))
                })?;
                Ok(Self::Perft(depth_usize))
            }
            _ => Err(EngineCommandError(format!("Invalid perft command"))),
        }
    }
}

#[derive(Debug)]
pub struct EngineCommandError(String);

impl std::fmt::Display for EngineCommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UCI Command Error! Reason: {}", self.0)
    }
}

impl std::error::Error for EngineCommandError {}
