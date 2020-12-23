use std::{env, fs};
use std::str::FromStr;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");
    let input: Vec<i64> = input_raw
        .lines()
        .map(|l| i64::from_str(l).unwrap())
        .collect();

    'outer: for cur in 25..input.len() {
        // Just loop over every pair in the previous 25. Horribly inefficient.
        for i in &input[cur-25..cur] {
            for j in &input[cur-25..cur] {
                if i + j == input[cur] {
                    continue 'outer;
                }
            }
        }
        println!("unmatched {}@{}", input[cur], cur);
        break;
    }
}
