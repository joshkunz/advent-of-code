use std::env;
use std::fs;
use std::num;
use std::str::FromStr;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
struct Error(String);

impl From<String> for Error {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for Error {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<num::ParseIntError> for Error {
    fn from(e: num::ParseIntError) -> Self {
        Self(e.to_string())
    }
}

#[derive(Debug)]
struct Rule {
    pos1: usize,
    pos2: usize,
    code: char,
}

impl Rule {
    fn parse(def: &str) -> Result<Self> {
        let parts: Vec<&str> = def.split(" ").collect();
        if parts.len() != 2 {
            return Err(format!("Invalid # of parts {} (want 2): {:?}", parts.len(), parts).into());
        }

        let chars: Vec<char> = parts[1].chars().collect();
        if chars.len() != 1 {
            return Err(format!("Invalid # of chars {} (want 1): {:?}", chars.len(), chars).into());
        }
        let code = chars[0];

        let range_parts: Vec<&str> = parts[0].split("-").collect();
        if range_parts.len() != 2 {
            return Err(format!("Invalid # of range parts {} (want 2): {:?}", range_parts.len(), range_parts).into());
        }

        return Ok(Rule{
            pos1: usize::from_str(range_parts[0])? - 1,
            pos2: usize::from_str(range_parts[1])? - 1,
            code: code,
        });
    }

    fn valid(&self, s: &str) -> bool {
        let chars: Vec<char> = s.chars().collect();

        let match_p1 = if chars.len() > self.pos1 {
            chars[self.pos1] == self.code
        } else {
            false
        };
        
        let match_p2 = if chars.len() > self.pos2 {
            chars[self.pos2] == self.code
        } else {
            false
        };

        // xor. Exactly one must match.
        return match_p1 != match_p2;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let mut cnt: i64 = 0;
    for l in input_raw.lines() {
        let parts: Vec<&str> = l.trim().split(": ").collect();
        let rule_raw = &parts[0];
        let pass = &parts[1];
        if Rule::parse(rule_raw).unwrap().valid(pass) {
            cnt += 1;
        }
    }
    println!("Valid passwords: {}", cnt);
}
