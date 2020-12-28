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
struct Position {
    // Positive is north, negative is south.
    ns: i64,
    // Positive is east, negative is west.
    ew: i64,
}

#[derive(Debug)]
struct Ship {
    waypoint: Position,
    loc: Position,
}

impl Ship {
    fn new() -> Self {
        Ship{
            waypoint: Position{ns: 1, ew: 10},
            loc: Position{ns: 0, ew: 0},
        }
    }

    fn waypoint_left(&mut self, deg: i64) {
        let mut deg = deg;
        while deg > 0 {
            self.waypoint = Position{
                ns: self.waypoint.ew,
                ew: -self.waypoint.ns
            };
            deg -= 90;
        }
    }

    fn waypoint_right(&mut self, deg: i64) {
        let mut deg = deg;
        while deg > 0 {
            self.waypoint = Position{
                ns: -self.waypoint.ew,
                ew: self.waypoint.ns,
            };
            deg -= 90;
        }
    }

    fn step(&mut self, i: &Inst) {
        match i {
            Inst::Navigate(d, v) => match d {
                Dir::North => self.waypoint.ns += v,
                Dir::South => self.waypoint.ns -= v,
                Dir::East => self.waypoint.ew += v,
                Dir::West => self.waypoint.ew -= v,
            },
            Inst::Left(v) => self.waypoint_left(*v),
            Inst::Right(v) => self.waypoint_right(*v),
            Inst::Forward(v) => {
                self.loc = Position{
                    ns: self.loc.ns + (self.waypoint.ns * v),
                    ew: self.loc.ew + (self.waypoint.ew * v),
                }
            }
        }
    }

    fn manhattan_dist(&self) -> i64 {
        self.loc.ns.abs() + self.loc.ew.abs()
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
