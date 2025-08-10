use std::str::{FromStr, SplitWhitespace};

fn parse<T: FromStr>(tokens: &mut SplitWhitespace) -> Result<T, &'static str> {
    tokens
        .next()
        .ok_or("Missing value for time control!")?
        .parse::<T>()
        .map_err(|_| "Invalid value for time control!")
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimeControl {
    Infinite,

    FixedDepth(usize),

    FixedNodes(u64),

    FixedTime(u64),

    Mate(usize),

    Variable {
        wtime: u64,

        btime: u64,
        winc: Option<u64>,
        binc: Option<u64>,
        movestogo: Option<u16>,
    },
}

#[derive(Debug, Default)]
struct TimeControlBuilder {
    wtime: Option<u64>,
    btime: Option<u64>,
    winc: Option<u64>,
    binc: Option<u64>,
    movestogo: Option<u16>,
}

impl TimeControlBuilder {
    fn new() -> Self {
        Self::default()
    }

    fn wtime(&mut self, val: u64) -> &mut Self {
        self.wtime = Some(val);
        self
    }

    fn btime(&mut self, val: u64) -> &mut Self {
        self.btime = Some(val);
        self
    }

    fn winc(&mut self, val: u64) -> &mut Self {
        self.winc = Some(val);
        self
    }

    fn binc(&mut self, val: u64) -> &mut Self {
        self.binc = Some(val);
        self
    }

    fn movestogo(&mut self, val: u16) -> &mut Self {
        self.movestogo = Some(val);
        self
    }

    fn build(self) -> Result<TimeControl, &'static str> {
        match (self.wtime, self.btime) {
            (Some(wt), Some(bt)) if wt > 0 && bt > 0 => Ok(TimeControl::Variable {
                wtime: wt,
                btime: bt,
                winc: self.winc,
                binc: self.binc,
                movestogo: self.movestogo,
            }),
            _ => Err("Unknown time control command or format"),
        }
    }
}

impl FromStr for TimeControl {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();

        match tokens.next() {
            Some("infinite") => Ok(TimeControl::Infinite),
            Some("depth") => Ok(TimeControl::FixedDepth(parse(&mut tokens)?)),
            Some("nodes") => Ok(TimeControl::FixedNodes(parse(&mut tokens)?)),
            Some("mate") => Ok(TimeControl::Mate(parse(&mut tokens)?)),
            Some("movetime") => Ok(TimeControl::FixedTime(parse(&mut tokens)?)),

            _ => Self::parse_variable(&mut s.split_whitespace()),
        }
    }
}

impl TimeControl {
    fn parse_variable(tokens: &mut SplitWhitespace) -> Result<TimeControl, &'static str> {
        let mut builder = TimeControlBuilder::new();

        while let Some(key) = tokens.next() {
            match key {
                "wtime" => builder.wtime(parse::<i64>(tokens)?.max(0) as u64),
                "btime" => builder.btime(parse::<i64>(tokens)?.max(0) as u64),
                "winc" => builder.winc(parse(tokens)?),
                "binc" => builder.binc(parse(tokens)?),
                "movestogo" => builder.movestogo(parse(tokens)?),
                _ => return Err("Unknown or unsupported parameter in go command"),
            };
        }

        builder.build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_infinite() {
        assert_eq!("infinite".parse::<TimeControl>(), Ok(TimeControl::Infinite));
    }

    #[test]
    fn test_parse_depth() {
        assert_eq!(
            "depth 8".parse::<TimeControl>(),
            Ok(TimeControl::FixedDepth(8))
        );
        assert_eq!(
            "depth".parse::<TimeControl>(),
            Err("Missing value for time control!")
        );
        assert_eq!(
            "depth eight".parse::<TimeControl>(),
            Err("Invalid value for time control!")
        );
    }

    #[test]
    fn test_parse_nodes() {
        assert_eq!(
            "nodes 100000".parse::<TimeControl>(),
            Ok(TimeControl::FixedNodes(100000))
        );
        assert_eq!(
            "nodes".parse::<TimeControl>(),
            Err("Missing value for time control!")
        );
        assert_eq!(
            "nodes الكثير".parse::<TimeControl>(),
            Err("Invalid value for time control!")
        );
    }

    #[test]
    fn test_parse_mate() {
        assert_eq!("mate 5".parse::<TimeControl>(), Ok(TimeControl::Mate(5)));
        assert_eq!(
            "mate".parse::<TimeControl>(),
            Err("Missing value for time control!")
        );
        assert_eq!(
            "mate -3".parse::<TimeControl>(),
            Err("Invalid value for time control!")
        );
    }

    #[test]
    fn test_parse_movetime() {
        assert_eq!(
            "movetime 5000".parse::<TimeControl>(),
            Ok(TimeControl::FixedTime(5000))
        );
        assert_eq!(
            "movetime".parse::<TimeControl>(),
            Err("Missing value for time control!")
        );
        assert_eq!(
            "movetime 1.5".parse::<TimeControl>(),
            Err("Invalid value for time control!")
        );
    }

    #[test]
    fn test_parse_go_variable_simple() {
        let expected = TimeControl::Variable {
            wtime: 10000,
            btime: 9000,
            winc: None,
            binc: None,
            movestogo: None,
        };
        assert_eq!(
            "wtime 10000 btime 9000".parse::<TimeControl>(),
            Ok(expected)
        );
    }

    #[test]
    fn test_parse_go_variable_all_params() {
        let expected = TimeControl::Variable {
            wtime: 300000,
            btime: 295000,
            winc: Some(2000),
            binc: Some(2000),
            movestogo: Some(40),
        };
        let input = "wtime 300000 btime 295000 winc 2000 binc 2000 movestogo 40";
        assert_eq!(input.parse::<TimeControl>(), Ok(expected));
    }

    #[test]
    fn test_parse_go_variable_reordered() {
        let expected = TimeControl::Variable {
            wtime: 300000,
            btime: 295000,
            winc: Some(2000),
            binc: Some(2000),
            movestogo: Some(40),
        };

        let input = "movestogo 40 winc 2000 wtime 300000 binc 2000 btime 295000";
        assert_eq!(input.parse::<TimeControl>(), Ok(expected));
    }

    #[test]
    fn test_parse_go_variable_with_only_some_optional() {
        let expected = TimeControl::Variable {
            wtime: 120000,
            btime: 118000,
            winc: Some(1000),
            binc: None,
            movestogo: None,
        };
        let input = "wtime 120000 btime 118000 winc 1000";
        assert_eq!(input.parse::<TimeControl>(), Ok(expected));

        let expected2 = TimeControl::Variable {
            wtime: 60000,
            btime: 59000,
            winc: None,
            binc: None,
            movestogo: Some(10),
        };
        let input2 = "wtime 60000 btime 59000 movestogo 10";
        assert_eq!(input2.parse::<TimeControl>(), Ok(expected2));
    }

    #[test]
    fn test_parse_go_variable_missing_required() {
        assert_eq!(
            "wtime 10000".parse::<TimeControl>(),
            Err("Unknown time control command or format")
        );
        assert_eq!(
            "btime 9000 winc 100".parse::<TimeControl>(),
            Err("Unknown time control command or format")
        );
        assert_eq!(
            "winc 100 binc 100".parse::<TimeControl>(),
            Err("Unknown time control command or format")
        );
        assert_eq!(
            "go".parse::<TimeControl>(),
            Err("Unknown or unsupported parameter in go command")
        );
    }

    #[test]
    fn test_parse_go_variable_invalid_value() {
        assert_eq!(
            "wtime ten btime 9000".parse::<TimeControl>(),
            Err("Invalid value for time control!")
        );
        assert_eq!(
            "wtime 10000 btime 9k".parse::<TimeControl>(),
            Err("Invalid value for time control!")
        );
        assert_eq!(
            "wtime 10000 btime 9000 movestogo forty".parse::<TimeControl>(),
            Err("Invalid value for time control!")
        );
        assert_eq!(
            "wtime 10000 btime 9000 winc -50".parse::<TimeControl>(),
            Err("Invalid value for time control!")
        );
    }

    #[test]
    fn test_parse_go_variable_missing_value() {
        assert_eq!(
            "wtime 10000 btime".parse::<TimeControl>(),
            Err("Missing value for time control!")
        );
        assert_eq!(
            "wtime".parse::<TimeControl>(),
            Err("Missing value for time control!")
        );
        assert_eq!(
            "wtime 10000 btime 9000 winc".parse::<TimeControl>(),
            Err("Missing value for time control!")
        );
    }

    #[test]
    fn test_parse_go_variable_unknown_param() {
        assert_eq!(
            "wtime 10000 btime 9000 unknown 123".parse::<TimeControl>(),
            Err("Unknown or unsupported parameter in go command")
        );
        assert_eq!(
            "go ponder wtime 10000 btime 9000".parse::<TimeControl>(),
            Err("Unknown or unsupported parameter in go command")
        );

        assert_eq!(
            "wtime 10000 btime 9000 searchmoves e2e4".parse::<TimeControl>(),
            Err("Unknown or unsupported parameter in go command")
        );
    }

    #[test]
    fn test_parse_empty_string() {
        assert_eq!(
            "".parse::<TimeControl>(),
            Err("Unknown time control command or format")
        );
    }

    #[test]
    fn test_parse_unknown_command() {
        assert_eq!(
            "ponder".parse::<TimeControl>(),
            Err("Unknown or unsupported parameter in go command")
        );
        assert_eq!(
            "startpos".parse::<TimeControl>(),
            Err("Unknown or unsupported parameter in go command")
        );
    }
}
