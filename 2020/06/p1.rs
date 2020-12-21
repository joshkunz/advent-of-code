use std::{env, fs};
use std::collections::HashSet;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let mut counts: i64 = 0;
    for group_raw in input_raw.split("\n\n") {
        let mut answered: HashSet<char> = HashSet::new();
        for line in group_raw.lines() {
            let set = line.chars().collect();
            answered = answered.union(&set).map(|v| *v).collect();
        }
        counts += answered.len() as i64;
    }

    println!("sum: {}", counts);
}
