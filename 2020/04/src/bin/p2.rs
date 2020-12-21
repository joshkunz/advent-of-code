#[macro_use]
extern crate lazy_static;

use regex;
use std::collections::HashMap;
use std::str::FromStr;
use std::{env, fs, result};

struct Error(String);

impl<T: ToString> From<T> for Error {
    fn from(v: T) -> Self {
        return Error(v.to_string());
    }
}

type Result<T> = result::Result<T, Error>;

enum Height {
    Cm(i64),
    In(i64),
}

impl FromStr for Height {
    type Err = Error;

    fn from_str(s: &str) -> Result<Height> {
        if s.len() < 2 {
            return Err(Error::from("string too short"));
        }

        let val: Result<i64> = s[..(s.len() - 2)].parse().map_err(Error::from);
        if s.ends_with("in") {
            return val.map(Height::In);
        } else if s.ends_with("cm") {
            return val.map(Height::Cm);
        }
        return Err(format!("unrecognized suffix: {}", &s[(s.len() - 2)..]).into());
    }
}

struct HairColor(String);

impl FromStr for HairColor {
    type Err = Error;

    fn from_str(s: &str) -> Result<HairColor> {
        lazy_static! {
            static ref MATCHER: regex::Regex = regex::Regex::new(r"^#([a-f0-9]{6})$").unwrap();
        }

        let found = MATCHER 
            .captures(s)
            .and_then(|c| c.get(1))
            .ok_or(Error::from("did not match"))?;

        return Ok(HairColor(found.as_str().to_string()));
    }
}

enum EyeColor {
    Amber,
    Blue,
    Brown,
    Grey,
    Green,
    Hazel,
    Other,
}

impl FromStr for EyeColor {
    type Err = Error;

    fn from_str(s: &str) -> Result<EyeColor> {
        match s {
            "amb" => Ok(EyeColor::Amber),
            "blu" => Ok(EyeColor::Blue),
            "brn" => Ok(EyeColor::Brown),
            "gry" => Ok(EyeColor::Grey),
            "grn" => Ok(EyeColor::Green),
            "hzl" => Ok(EyeColor::Hazel),
            "oth" => Ok(EyeColor::Other),
            c => Err(Error::from(format!("unrecognized color: {}", c))),
        }
    }
}

fn valid(s: &str) -> bool {
    let mut items: HashMap<&str, &str> = HashMap::new();

    for item in s.split_whitespace() {
        let parts: Vec<&str> = item.split(":").collect();
        assert!(parts.len() == 2);
        items.insert(parts[0], parts[1]);
    }

    match items.get("byr").map(|v| i64::from_str(v)) {
        Some(Ok(year)) if year >= 1920 && year <= 2002 => (),
        _ => return false,
    };

    match items.get("iyr").map(|v| i64::from_str(v)) {
        Some(Ok(year)) if year >= 2010 && year <= 2020 => (),
        _ => return false,
    };

    match items.get("eyr").map(|v| i64::from_str(v)) {
        Some(Ok(year)) if year >= 2020 && year <= 2030 => (),
        _ => return false,
    };

    match items.get("eyr").map(|v| i64::from_str(v)) {
        Some(Ok(year)) if year >= 2020 && year <= 2030 => (),
        _ => return false,
    };

    match items.get("hgt").map(|v| Height::from_str(v)) {
        Some(Ok(Height::Cm(h))) if h >= 150 && h <= 193 => (),
        Some(Ok(Height::In(h))) if h >= 59 && h <= 76 => (),
        _ => return false,
    };

    match items.get("hcl").map(|v| HairColor::from_str(v)) {
        Some(Ok(_)) => (),
        _ => return false,
    };

    match items.get("ecl").map(|v| EyeColor::from_str(v)) {
        Some(Ok(_)) => (),
        _ => return false,
    };

    let pid = items
        .get("pid")
        .map(|v| {
            if v.len() != 9 {
                return false;
            }
            v.chars().all(|c| c.is_ascii_digit())
        });
    match pid {
        Some(true) => (),
        _ => return false,
    };

    // All checks passed.
    return true;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let mut count: i64 = 0;
    for record in input_raw.split("\n\n") {
        if valid(record) {
            count += 1;
        }
    }
    println!("Valid: {}", count);
}
