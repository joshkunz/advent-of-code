use std::{result, env, fs, fmt, mem};
use std::str::FromStr;
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
    Nop,
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
            "nop" => Ok(Instruction::Nop),
            c => Err(Error::from(format!("invalid code '{}'", c))),
        }
    }
}

impl Instruction {
    fn is_jmp(&self) -> bool {
        if let Instruction::Jmp(_) = self {
            return true;
        }
        return false;
    }
}

type Program = Vec<Instruction>;

#[derive(Debug,PartialEq,Eq)]
enum State {
    Running,
    Halted,
}

#[derive(Debug)]
struct Computer {
    pc: usize,
    acc: i64,
    state: State,
    program: Program,
}

impl From<Program> for Computer {
    fn from(p: Program) -> Self {
        Computer{
            pc: 0,
            acc: 0,
            state: State::Running,
            program: p,
        }
    }
}

impl fmt::Display for Computer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Computer(pc = {}, acc = {}, state = {:?})",
            self.pc, self.acc, self.state)
    }
}

impl Computer {
    fn step(&mut self) {
        if let State::Halted = self.state {
            return;
        }

        let i = &self.program[self.pc];
        match *i {
            Instruction::Nop => self.pc += 1,
            Instruction::Acc(v) => {
                self.acc += v;
                self.pc += 1;
            },
            Instruction::Jmp(o) => self.pc = ((self.pc as i64) + o) as usize,
        };
        if self.pc >= self.program.len() {
            self.state = State::Halted;
        }
    }

    fn reset(&mut self) {
        self.pc = 0;
        self.acc = 0;
        self.state = State::Running;
    }

    fn is_halted(&self) -> bool {
        return self.state == State::Halted;
    }

    fn halts(&mut self) -> bool {
        let mut visited: HashSet<usize> = HashSet::new();
        while !self.is_halted() && !visited.contains(&self.pc) {
            visited.insert(self.pc);
            self.step();
        }
        return self.is_halted();
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

    for idx in 0..c.program.len() {
        if !c.program[idx].is_jmp() {
            continue;
        }
        let mut tmp = Instruction::Nop;
        mem::swap(&mut c.program[idx], &mut tmp);
        c.reset();
        if c.halts() {
            println!("Nop@{} triggered halt: {}", idx, c);
            break;
        }
        mem::swap(&mut c.program[idx], &mut tmp);
    }
}
