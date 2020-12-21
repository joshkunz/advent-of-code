use std::{env, fs};
use std::collections::HashSet;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let mut counts: i64 = 0;
    for group_raw in input_raw.split("\n\n") {
        let mut ls = group_raw.lines();

        // First capture the response from the first line.
        let mut answered: HashSet<char> = ls.next().unwrap().chars().collect();

        for line in ls {
            let set = line.chars().collect();
            // Now remove any answers the current person hasn't answered.
            answered = answered.intersection(&set).map(|v| *v).collect();
        }
        counts += answered.len() as i64;
    }

    println!("sum: {}", counts);
}
