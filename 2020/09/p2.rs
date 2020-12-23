use std::{env, fs};
use std::str::FromStr;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");
    let input: Vec<i64> = input_raw
        .lines()
        .map(|l| i64::from_str(l).unwrap())
        .collect();

    let unmatched: i64;
    {
        let mut unmatched_o: Option<i64> = Option::None;
        'outer: for cur in 25..input.len() {
            // Just loop over every pair in the previous 25. Horribly inefficient.
            for i in &input[cur-25..cur] {
                for j in &input[cur-25..cur] {
                    if i + j == input[cur] {
                        continue 'outer;
                    }
                }
            }
            unmatched_o = Some(input[cur]);
            break;
        }
        // Here we assert that we reach L25;
        unmatched = unmatched_o.unwrap();
    }

    let mut trailer: usize = 0;
    let mut leader: usize = 1;

    while trailer < input.len()-2 {
        let s: i64 = input[trailer..leader].iter().sum();
        if s == unmatched {
            println!("Found range: {}..{}: Sum({:#?}) = {}",
                trailer, leader, &input[trailer..leader], s);
            let min = input[trailer..leader].iter().min().unwrap();
            let max = input[trailer..leader].iter().max().unwrap();
            println!("Min = {}, Max = {}", min, max);
            println!("Key = {}", min + max);
            break;
        } else if s < unmatched {
            leader += 1;
        } else if s > unmatched {
            trailer += 1;
        }
        if leader == trailer {
            leader += 1;
        }
        if leader > input.len()-1 {
            leader = input.len()-1;
        }
    }
}
