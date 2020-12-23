use std::{result, env, fs};
use std::str::FromStr;
use std::fmt;
use std::collections::HashSet;

#[derive(Debug,PartialEq,Eq)]
struct Error(String);

impl<T: ToString> From<T> for Error {
    fn from(v: T) -> Self {
        Error(v.to_string())
    }
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug,PartialEq,Eq)]
enum Instruction {
    Acc(i64),
    Jmp(i64),
    Nop(i64),
}

impl FromStr for Instruction {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let parts: Vec<&str> = s.split(" ").collect();
        if parts.len() != 2 {
            return Err(Error::from("invalid number of parts"));
        }
        let (code, arg_raw) = (parts[0], parts[1]);
        let arg: i64 = arg_raw.parse()?;

        match code {
            "acc" => Ok(Instruction::Acc(arg)),
            "jmp" => Ok(Instruction::Jmp(arg)),
            "nop" => Ok(Instruction::Nop(arg)),
            c => Err(Error::from(format!("invalid code '{}'", c))),
        }
    }
}

type Program = Vec<Instruction>;

#[derive(Debug)]
struct Computer {
    pc: u64,
    acc: i64,
    program: Program,
}

impl From<Program> for Computer {
    fn from(p: Program) -> Self {
        Computer{
            pc: 0,
            acc: 0,
            program: p,
        }
    }
}

impl fmt::Display for Computer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Computer(pc = {}, acc = {})", self.pc, self.acc)
    }
}

impl Computer {
    fn step(&mut self) {
        let i = &self.program[self.pc as usize];
        if let Instruction::Jmp(offset) = i {
            self.pc = ((self.pc as i64) + offset) as u64;
            return;
        }
        match *i {
            Instruction::Nop(_) => (),
            Instruction::Acc(v) => self.acc += v,
            // Handled above.
            Instruction::Jmp(_) => unreachable!(),
        };
        self.pc += 1;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let mut c: Computer = Computer::from(
        input_raw
            .lines()
            .map(|l| Instruction::from_str(l).unwrap())
            .collect::<Program>()
    );

    let mut visited: HashSet<u64> = HashSet::new();
    while !visited.contains(&c.pc) {
        visited.insert(c.pc);
        c.step();
    }

    println!("Halted at: {}", c);
}
