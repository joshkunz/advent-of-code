use std::{env, fs};

fn parse(s: &str) -> Vec<&str> {
    let mut result: Vec<&str> = Vec::new();
    for item in s.split_whitespace() {
        let parts: Vec<&str> = item.split(":").collect();
        assert!(parts.len() == 2);
        result.push(&parts[0]);
    }
    return result;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    // cid is optional, so not included.
    let required = {
        let mut required = vec!["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
        required.sort();
        required
    };

    let mut valid: i64 = 0;
    for record in input_raw.split("\n\n") {
        let mut parsed = parse(record);
        parsed.sort();
        if let Some(idx) = parsed.iter().position(|x| *x == "cid") {
            parsed.remove(idx);
        }
        if parsed == required {
            valid += 1;
        }
    }
    println!("Valid: {}", valid);
}
