use anyhow::Result;
use anyhow;
use std::io::Read;
use std::io;
use std::str::FromStr;

fn main() {
    let mut input: String = String::default();
    io::stdin().read_to_string(&mut input).unwrap();

    println!("Solution 1: {}", part1(&input).expect("part 1"));
}

fn part1(input: &str) -> Result<i64> {
    let insts = parse(input)?;
    let mut c = Puter::new();
    let out = c.simulate(&insts);
    //println!("{:#?}", out);
    return Ok(out.iter().map(|s| s.value).sum());
}

#[derive(Debug,PartialEq)]
enum Inst {
    Noop,
    AddX(i64),
}

impl FromStr for Inst {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let parts: Vec<&str> = s.split_whitespace().collect();
        match parts[..] {
            ["noop"] => Ok(Inst::Noop),
            ["addx", size] => Ok(Inst::AddX(size.parse()?)),
            _ => anyhow::bail!("no match for {:#?}", parts),
        }
    }
}

fn parse(input: &str) -> Result<Vec<Inst>> {
    let mut out: Vec<Inst> = Vec::new();
    for l in input.lines() {
        out.push(l.parse()?);
    }
    return Ok(out);
}

#[derive(Debug,Default)]
struct Puter {
    pc: usize,
    x: i64,
    cycle: usize,
}

#[derive(Debug,PartialEq)]
struct Sample {
    cycle: usize,
    x: i64,
    value: i64,
}

impl Puter {
    fn new() -> Self {
        return Puter{
            pc: 0,
            x: 1,
            cycle: 1,
        };
    }

    fn simulate(&mut self, insts: &Vec<Inst>) -> Vec<Sample> {
        let mut out: Vec<Sample> = Vec::new();

        let mut ctr: usize = 0;
        let mut sample_wait: usize = 20;
        while self.pc < insts.len() {
            sample_wait -= 1;
            if sample_wait == 0 {
                let s = Sample{
                    cycle: self.cycle,
                    x: self.x,
                    value: (self.cycle as i64) * self.x
                };
                out.push(s);
                sample_wait = 40;
            }

            let inst = &insts[self.pc];
            self.cycle += 1;
            ctr += 1;


            match inst {
                Inst::Noop => {
                    self.pc += 1;
                    ctr = 0;
                },
                Inst::AddX(v) if ctr == 2 => {
                    self.pc += 1;
                    self.x += v;
                    ctr = 0;
                }
                // Nothing to do. Don't increment pc, and don't change X.
                Inst::AddX(..) => (), 
            }
        }
        return out;
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inst_parse() {
        assert_eq!(Inst::from_str("noop").unwrap(), Inst::Noop);
        assert_eq!(Inst::from_str("addx 1").unwrap(), Inst::AddX(1));
        assert_eq!(Inst::from_str("addx -5").unwrap(), Inst::AddX(-5));
        assert!(Inst::from_str("invalid").is_err());

    }
}
