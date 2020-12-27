use std::{env, fs};
use std::str::FromStr;

#[derive(Debug,PartialEq,Eq,PartialOrd,Ord)]
struct Adapter(i64);

impl Adapter {
    fn output(&self) -> i64 {
        let Adapter(v) = self;
        return *v;
    }
}

impl From<i64> for Adapter {
    fn from(v: i64) -> Self {
        Adapter(v)
    }
}

fn solve_chain(mut vs: Vec<Adapter>) -> i64 {
    vs.sort();
    vs.push((vs[vs.len()-1].output() + 3).into());
    let mut prev = 0;
    let mut j1 = 0;
    let mut j3 = 0;
    for i in vs {
        let o = i.output();
        let diff = o - prev;
        println!("Comparing {}->{}, diff: {}", prev, o, diff);
        if diff == 1 {
            j1 += 1;
        } else if diff == 3 {
            j3 += 1;
        }
        prev = o;
    }
    println!("1-jolt: {}, 3-jolt: {}", j1, j3);
    return j1 * j3;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");
    let input: Vec<Adapter> = input_raw
        .lines()
        .map(|l| Adapter::from(i64::from_str(l.trim()).unwrap()))
        .collect();

    println!("Solution: {}", solve_chain(input));
}
