use std::{env, fs};
use std::str::FromStr;

#[derive(Debug)]
struct Departure {
    id: i64,
    leaving_in: i64,
}

impl Departure {
    fn solution(&self) -> i64 {
        return self.id * self.leaving_in;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");
    let lines: Vec<&str> = input_raw.lines().collect();

    let earliest: i64 = lines[0].parse().unwrap();
    let busses: Vec<i64> = lines[1]
        .split(",")
        .filter(|v| *v != "x")
        .map(|v| i64::from_str(v).unwrap())
        .collect();

    println!("Earliest: {}", earliest);
    println!("Busses: {:?}", busses);

    let mut leaving_in: Vec<Departure> = busses
        .iter()
        .map(|bus| Departure{
            id: *bus,
            leaving_in: *bus - (earliest % *bus),
        })
        .collect();
    leaving_in.sort_by(|a, b| a.leaving_in.cmp(&b.leaving_in));

    println!("Soonest: {:#?}", &leaving_in[0]);
    println!("Time: {}", earliest + leaving_in[0].leaving_in);
    println!("Solution: {}", leaving_in[0].solution());
}
