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
    min: i64,
    max: i64,
    code: char,
}

impl ToString for Rule {
    fn to_string(&self) -> String {
        format!("{}-{} {}", self.min, self.max, self.code)
    }
}

fn valid(r: &Rule, s: &str) -> bool {
    let mut cnt: i64 = 0;
    for c in s.chars() {
        if c != r.code {
            continue;
        }
        cnt += 1;
        if cnt > r.max {
            return false;
        }
    }
    return r.min <= cnt;
}

fn parse_rule(def: &str) -> Result<Rule> {
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
        min: i64::from_str(range_parts[0])?,
        max: i64::from_str(range_parts[1])?,
        code: code,
    });
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let mut cnt: i64 = 0;
    for l in input_raw.lines() {
        let parts: Vec<&str> = l.trim().split(": ").collect();
        let rule_raw = &parts[0];
        let pass = &parts[1];
        if valid(&parse_rule(rule_raw).unwrap(), pass) {
            cnt += 1;
        }
    }
    println!("Valid passwords: {}", cnt);
}
