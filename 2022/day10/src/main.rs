use anyhow::Result;
use anyhow;
use std::io::Read;
use std::io;
use std::str::FromStr;

fn main() {
    let mut input: String = String::default();
    io::stdin().read_to_string(&mut input).unwrap();

    println!("Solution 1: {}", part1(&input).expect("part 1"));
    println!("Solution 2:");
    part2(&input).expect("part 2");
}

fn part1(input: &str) -> Result<i64> {
    let insts = parse(input)?;
    let mut c = Puter::new();
    let out = c.sample(&insts);
    //println!("{:#?}", out);
    return Ok(out.iter().map(|s| s.value).sum());
}

fn part2(input: &str) -> Result<()> {
    let insts = parse(input)?;
    let mut c = Puter::new();
    c.display(&insts);
    //println!("{:#?}", out);
    return Ok(());
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
    scan: usize,
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
            scan: 0,
        };
    }

    fn run<F>(&mut self, insts: &Vec<Inst>, mut f: F)
    where
        F: FnMut(&mut Self),
    {
        let mut ctr: usize = 0;
        while self.pc < insts.len() {
            f(self);

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
    }

    fn sample(&mut self, insts: &Vec<Inst>) -> Vec<Sample> {
        let mut out: Vec<Sample> = Vec::new();
        let mut sample_wait: usize = 20;

        self.run(insts, |p| {
            sample_wait -= 1;
            if sample_wait == 0 {
                let s = Sample{
                    cycle: p.cycle,
                    x: p.x,
                    value: (p.cycle as i64) * p.x
                };
                out.push(s);
                sample_wait = 40;
            }
        });

        return out;
    }

    fn is_lit(&self) -> bool {
        if self.x < 0 {
            return false;
        }
        let loc = self.scan % 40;
        return loc+1 == (self.x as usize)
            || loc == (self.x as usize)
            || loc == (self.x as usize)+1;
    }

    fn display(&mut self, insts: &Vec<Inst>)  {
        self.run(insts, |p| {
            if p.scan != 0 && p.scan % 40 == 0 {
                println!();
            }
            if p.is_lit() {
                print!("#");
            } else {
                print!(".");
            }
            p.scan += 1;
        })
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
