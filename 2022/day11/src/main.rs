use anyhow::*;
use std::io::Read;
use std::io;
use std::str::FromStr;
use std::collections::VecDeque;
use std::cell::RefCell;

fn main() {
    let mut input: String = String::default();
    io::stdin().read_to_string(&mut input).unwrap();

    let monkeys = parse(&input).expect("works!");

    // Every modulus in the input is prime, so the least common multiple over
    // all the inputs is the product of those primes. The lcm is evenly
    // divisble by the modulus of every monkey, so restricting the worry
    // by the modulus of the lcm does not impact the divisiblity of any
    // monkey's modulus.
    let lcm = monkeys.iter().map(|v| v.modulus).reduce(|a, b| a * b).unwrap();

    println!("Part 1: {}",
             monkey_business(monkeys.clone(), Rounds(20), Constraint::Div(3)));
    println!("Part 2: {}",
             monkey_business(monkeys.clone(), Rounds(10000), Constraint::Mod(lcm)));
}

struct Rounds(usize);

enum Constraint {
    Div(i64),
    Mod(i64)
}

fn monkey_business(jungle: Vec<Monkey>, Rounds(rounds): Rounds, constraint: Constraint) -> i64 {
    let mut inspections: Vec<usize> = vec![0; jungle.len()];
    for _round in 0..rounds {
        for (idx, monkey) in jungle.iter().enumerate() {
            while monkey.items.borrow().len() > 0 {
                inspections[idx] += 1;

                let old = monkey.items.borrow_mut().pop_front().unwrap();
                let new = match constraint {
                    Constraint::Div(d) => monkey.op.worry(old) / d,
                    Constraint::Mod(m) => monkey.op.worry(old) % m,
                };
                let target = if new % monkey.modulus == 0 {
                    monkey.divisible_target
                } else {
                    monkey.else_target
                };

                jungle[target].items.borrow_mut().push_back(new);
            }
        }
    }
    inspections.sort();
    inspections.reverse();
    return (inspections[0] * inspections[1]) as i64;
}

#[derive(Debug,PartialEq,Clone,Copy)]
enum Value {
    Old,
    Imm(i64),
}

impl FromStr for Value {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        Ok(match s {
            "old" => Value::Old,
            v => Value::Imm(v.parse()?),
        })
    }
}

#[derive(Debug,PartialEq,Clone,Copy)]
enum Op {
    Plus,
    Mul,
}

impl FromStr for Op {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Op> {
        Ok(match s {
            "+" => Op::Plus,
            "*" => Op::Mul,
            _ => anyhow::bail!("unknown op {}", s),
        })
    }
}

#[derive(Debug,PartialEq,Clone,Copy)]
struct Operation {
    left: Value,
    op: Op,
    right: Value,
}

impl Operation {
    fn worry(&self, old: i64) -> i64 {
        let left = match self.left {
            Value::Old => old,
            Value::Imm(v) => v,
        };
        let right = match self.right {
            Value::Old => old,
            Value::Imm(v) => v,
        };
        match self.op {
            Op::Plus => left + right,
            Op::Mul => left * right,
        }
    }
}

#[derive(Debug,PartialEq,Clone)]
struct Monkey {
    items: RefCell<VecDeque<i64>>,
    op: Operation,
    modulus: i64 ,
    divisible_target: usize,
    else_target: usize,
}

fn parse_operation(s: &str) -> Result<Operation> {
    let s =
        s.trim()
        .strip_prefix("Operation: new = ")
        .ok_or(anyhow!("bad operation prefix"))?;
    let vals: Vec<&str> = s.split(" ").collect();
    let (left, op, right) = (vals[0], vals[1], vals[2]); 
    Ok(Operation{
        left: left.parse()?,
        op: op.parse()?,
        right: right.parse()?,
    })
}

fn parse(input: &str) -> Result<Vec<Monkey>> {
    let mut out: Vec<Monkey> = Vec::new();
    for group in input.lines().collect::<Vec<&str>>().split(|&e| e == "") {
        if group.len() < 6 {
            // handle any space at the end.
            break;
        }
        /* Don't consider the ID for now.
        let id = group[0]
            .split_once(' ')
            .ok_or(anyhow!("name without space"))
            .and_then(|s| {
                s.1.strip_suffix(":")
                    .unwrap_or(s.1)
                    .parse::<usize>().or_else(|e| bail!(e))
            })?;
        */
        let starting: VecDeque<i64> = group[1]
            .trim()
            .strip_prefix("Starting items: ")
            .ok_or(anyhow!("invalid starting prefix"))?
            .split(", ")
            .map(|s| s.parse::<i64>().or_else(|e| bail!(e)))
            .collect::<Result<VecDeque<i64>>>()?;
        let op: Operation = parse_operation(group[2].trim())?;
        let modulus: i64 = group[3]
            .trim()
            .strip_prefix("Test: divisible by ")
            .ok_or(anyhow!("invalid modulus prefix"))?
            .parse()?;
        let if_true: usize = group[4]
            .trim()
            .strip_prefix("If true: throw to monkey ")
            .ok_or(anyhow!("invalid true prefix"))?
            .parse()?;
        let if_false: usize = group[5]
            .trim()
            .strip_prefix("If false: throw to monkey ")
            .ok_or(anyhow!("invalid false prefix"))?
            .parse()?;
        let m = Monkey{
            items: starting.into(),
            op: op,
            modulus: modulus,
            divisible_target: if_true,
            else_target: if_false,
        };
        out.push(m);
    } 
    return Ok(out);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_fromstr() {
        assert_eq!(Value::from_str("old").unwrap(), Value::Old);
        assert_eq!(Value::from_str("7").unwrap(), Value::Imm(7));
        assert!(Value::from_str("z").is_err());
    }

    #[test]
    fn test_parse() {
        let input = "
Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0".trim();
        let want: Vec<Monkey> = vec![ 
            Monkey{
                items: vec![54, 65, 75, 74],
                op: Operation{
                    left: Value::Old,
                    op: Op::Plus,
                    right: Value::Imm(6),
                },
                modulus: 19,
                divisible_target: 2,
                else_target: 0,
            },
        ];

        let got = parse(input).expect("parses");
        assert_eq!(want, got);
    }

    #[test]
    fn test_operation_worry() {
        assert_eq!(Operation{
            left: Value::Old,
            op: Op::Plus,
            right: Value::Old,
        }.worry(5), 10/3);
        assert_eq!(Operation{
            left: Value::Imm(0),
            op: Op::Mul,
            right: Value::Old,
        }.worry(100), 0);
    }
}
