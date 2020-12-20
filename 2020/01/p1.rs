use std::env;
use std::fs;
use std::str::FromStr;
use std::result::Result;

fn solve(input: &Vec<i64>) -> Result<(i64, i64), String> {
    for i in input.into_iter() {
        for j in input.into_iter() {
            if i + j == 2020 {
                return Ok((*i, *j));
            }
        }
    }
    return Err(String::from("No result found"));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let mut input: Vec<i64> = Vec::new();
    for elem in input_raw.split_whitespace() {
        input.push(i64::from_str(elem).expect("invalid number"));
    }

    let nums = solve(&input).unwrap();

    println!("Solution: {:?}", nums.0 * nums.1);
}
