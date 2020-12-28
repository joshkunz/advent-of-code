use std::{result, env, fs};
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
struct Error(String);

impl<T: ToString> From<T> for Error {
    fn from(v: T) -> Self {
        Error(v.to_string())
    }
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Dir {
    North,
    South,
    East,
    West,
}

impl FromStr for Dir {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "N" => Ok(Dir::North),
            "S" => Ok(Dir::South),
            "E" => Ok(Dir::East),
            "W" => Ok(Dir::West),
            _ => Err(Error::from(format!("Unknown direction '{}'", s))),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Inst {
    Navigate(Dir, i64),
    Left(i64),
    Right(i64),
    Forward(i64),
}

impl FromStr for Inst {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let code = &s[..1];
        let val: i64 = s[1..].parse()?;

        match code {
            "N" | "S" | "E" | "W" => Ok(Inst::Navigate(Dir::from_str(code)?, val)),
            "L" => Ok(Inst::Left(val)),
            "R" => Ok(Inst::Right(val)),
            "F" => Ok(Inst::Forward(val)),
            _ => Err(Error::from(format!("Unknown code '{}'", code))),
        }
    }
}

#[derive(Debug)]
struct Ship {
    dir: Dir,
    // Positive is north, negative is south.
    ns: i64,
    // Positive is north, negative is south.
    ew: i64,
}

impl Ship {
    fn new() -> Self {
        Ship{
            dir: Dir::East,
            ns: 0,
            ew: 0,
        }
    }

    fn turn_left(&mut self, deg: i64) {
        let mut deg = deg;
        while deg > 0 {
            self.dir = match self.dir {
                Dir::North => Dir::West,
                Dir::West => Dir::South,
                Dir::South => Dir::East,
                Dir::East => Dir::North,
            };
            deg -= 90;
        }
    }

    fn turn_right(&mut self, deg: i64) {
        let mut deg = deg;
        while deg > 0 {
            self.dir = match self.dir {
                Dir::North => Dir::East,
                Dir::East => Dir::South,
                Dir::South => Dir::West,
                Dir::West => Dir::North,
            };
            deg -= 90;
        }
    }

    fn step(&mut self, i: &Inst) {
        match i {
            Inst::Navigate(d, v) => match d {
                Dir::North => self.ns += v,
                Dir::South => self.ns -= v,
                Dir::East => self.ew += v,
                Dir::West => self.ew -= v,
            },
            Inst::Left(v) => self.turn_left(*v),
            Inst::Right(v) => self.turn_right(*v),
            Inst::Forward(v) => self.step(&Inst::Navigate(self.dir, *v)),
        }
    }

    fn manhattan_dist(&self) -> i64 {
        self.ns.abs() + self.ew.abs()
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    assert_eq!(Inst::from_str("F10"), Ok(Inst::Forward(10)));
    assert_eq!(Inst::from_str("N3"), Ok(Inst::Navigate(Dir::North, 3)));
    assert_eq!(Inst::from_str("F7"), Ok(Inst::Forward(7)));
    assert_eq!(Inst::from_str("R90"), Ok(Inst::Right(90)));

    let input: Vec<Inst> = input_raw
        .lines()
        .map(|l| Inst::from_str(l).unwrap())
        .collect();

    let mut s = Ship::new();
    println!("Start: {:#?}", s);

    for i in &input {
        s.step(i);
    }

    println!("Final: {:#?}", s);
    println!("Manhattan: {}", s.manhattan_dist());
}
