use anyhow::*;
use anyhow;
use std::collections::HashSet;
use std::io::Read;
use std::io;
use std::str::FromStr;

fn main() {
    let mut input: String = String::default();
    io::stdin().read_to_string(&mut input).unwrap();

    let puzzle = parse(&input).expect("failed parse");
    println!("Part1: {}", tail_positions(&puzzle).len());
}

#[derive(Debug,PartialEq)]
enum Dir {
    Right,
    Left,
    Up,
    Down,
}

impl TryFrom<char> for Dir {
    type Error = anyhow::Error;

    fn try_from(value: char) -> Result<Dir> {
        match value {
            'R' => Ok(Dir::Right),
            'L' => Ok(Dir::Left),
            'U' => Ok(Dir::Up),
            'D' => Ok(Dir::Down),
            _ => bail!("invalid direction: {}", value),
        }
    }
}

#[derive(Debug,PartialEq)]
struct Inst {
    dir: Dir,
    amount: usize,
}

impl FromStr for Inst {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Inst> {
        let (a, b) = s.split_once(" ").ok_or(anyhow!("cannot split: {}", s))?;
        return Ok(Inst{
            dir: a.chars().next().ok_or(anyhow!("no direction char in {}", s))?.try_into()?,
            amount: b.parse()?,
        });
    }
}

fn parse(input: &str) -> Result<Vec<Inst>> {
    let mut out: Vec<Inst> = Vec::new();
    for l in input.lines() {
        out.push(l.parse()?);
    }
    return Ok(out);
}

#[derive(Debug,PartialEq,Eq,Hash,Clone,Copy)]
struct Position{x: i64, y: i64}

fn tail_positions(insts: &Vec<Inst>) -> HashSet<Position> {
    let mut pos: HashSet<Position> = HashSet::new();

    let mut head = Position{x: 0, y: 0};
    let mut tail = Position{x: 0, y: 0};

    // Count the start position.
    pos.insert(tail);

    for Inst{dir, amount} in insts {
        for _ in 0..*amount {
            match dir {
                Dir::Left => head.x -= 1,
                Dir::Right => head.x += 1,
                Dir::Up => head.y += 1,
                Dir::Down => head.y -= 1,
            }

            let dx = head.x - tail.x;
            let dy = head.y - tail.y;

            match (dx, dy) {
                //  ..h..
                //  .....
                //  h.t.h
                //  .....
                //  ..h..
                (-2, 0) => tail.x -= 1,
                (2, 0) => tail.x += 1,
                (0, -2) => tail.y -= 1,
                (0, 2) => tail.y += 1,
                // .h.h.
                // .....
                // ..t..
                // .....
                // .h.h.
                (1, 2) => {
                    tail.x += 1;
                    tail.y += 1;
                },
                (1, -2) => {
                    tail.x += 1;
                    tail.y -= 1;
                },
                (-1, 2) => {
                    tail.x -= 1;
                    tail.y += 1;
                },
                (-1, -2) => {
                    tail.x -= 1;
                    tail.y -= 1;
                },
                // .....
                // h...h
                // ..t..
                // h...h
                // .....
                (2, 1) => {
                    tail.x += 1;
                    tail.y += 1;
                },
                (2, -1) => {
                    tail.x += 1;
                    tail.y -= 1;
                },
                (-2, 1) => {
                    tail.x -= 1;
                    tail.y += 1;
                },
                (-2, -1) => {
                    tail.x -= 1;
                    tail.y -= 1;
                },
                // Assume we're still adjacent
                _ => (),
            }

            /*
            println!("head {:#?}", head);
            println!("dx={}, dy={}", dx, dy);
            println!("tail {:#?}", tail);
            */

            if pos.insert(tail) {
                //println!("touched {:#?}", tail);
            }
            /*
            println!("----");
            */
        }
    }

    return pos;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dir_tryfrom() {
        assert_eq!(Dir::try_from('R').unwrap(), Dir::Right);
        assert_eq!(Dir::try_from('L').unwrap(), Dir::Left);
        assert_eq!(Dir::try_from('U').unwrap(), Dir::Up);
        assert_eq!(Dir::try_from('D').unwrap(), Dir::Down);
        assert!(Dir::try_from('X').is_err());
    }

    #[test]
    fn test_inst_fromstr() {
        assert_eq!(Inst::from_str("R 5").unwrap(), Inst{dir: Dir::Right, amount: 5});
    }
}
