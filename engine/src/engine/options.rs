use std::str::{FromStr, SplitWhitespace};

use super::command::UCICommandError;

#[derive(Debug, PartialEq, Eq)]
pub enum EngineOption {
    Chess960(bool),

    ClearHash,

    ResizeHash(usize),

    ResizeThreads(usize),

    #[cfg(feature = "tune")]
    SetTunable(String, String),
}

impl EngineOption {
    fn parse_value<T: FromStr>(
        option_name: &str,
        tokens: SplitWhitespace,
    ) -> Result<T, UCICommandError> {
        let value_str = tokens.collect::<Vec<&str>>().join(" ");

        if value_str.is_empty() {
            return Err(UCICommandError(format!(
                "Missing value for option '{}'",
                option_name
            )));
        }

        value_str
            .parse::<T>()
            .map_err(|_| UCICommandError(format!("Invalid value type")))
    }
}

impl TryFrom<(String, SplitWhitespace<'_>)> for EngineOption {
    type Error = UCICommandError;

    fn try_from((option_name, tokens): (String, SplitWhitespace)) -> Result<Self, UCICommandError> {
        let option = match option_name.to_ascii_lowercase().as_str() {
            "uci_chess960" => Self::Chess960(Self::parse_value(&option_name, tokens)?),
            "clear hash" => Self::ClearHash,
            "hash" => Self::ResizeHash(Self::parse_value(&option_name, tokens)?),
            "threads" => Self::ResizeThreads(Self::parse_value(&option_name, tokens)?),

            #[cfg(not(feature = "tune"))]
            _ => {
                return Err(UCICommandError(format!(
                    "Unknown option name: '{}'",
                    option_name
                )));
            }

            #[cfg(feature = "tune")]
            _ => Self::SetTunable(option_name, tokens.collect::<Vec<&str>>().join(" ")),
        };

        Ok(option)
    }
}
