use std::{result, env, fs};
use std::str::FromStr;

type Result<T> = result::Result<T, String>;

#[derive(Debug,PartialEq)]
struct Seat {
    row: usize,
    col: usize,
}

impl Seat {
    fn id(&self) -> usize {
        return (self.row * 8) + self.col;
    }
}

fn binary_locate(id: &str, upper: char, lower: char) -> usize {
    assert!(id.len() > 0);
    let mut low: usize = 0;
    let mut high: usize = 2_usize.pow(id.len() as u32) - 1;

    for c in id[..id.len()-1].chars() {
        assert!(c == upper || c == lower);
        // +1 to round up.
        let half = ((high - low) / 2) + 1;
        if c == upper {
            low += half;
        } else if c == lower {
            high -= half;
        }
    }

    let last = id.chars().last().unwrap();
    if last == upper {
        return high;
    } else if last == lower {
        return low;
    }
    unreachable!("all chars must match upper or lower");
}

impl FromStr for Seat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self> {
        if s.len() != 10 {
            return Err(format!("invalid size {}", s.len()));
        }

        Ok(Seat{
            row: binary_locate(&s[..7], 'B', 'F'),
            col: binary_locate(&s[7..], 'R', 'L'),
        })
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let max = input_raw
        .lines()
        .map(|l| Seat::from_str(l).unwrap().id())
        .max()
        .unwrap();
    println!("Max ID: {}", max);
}
