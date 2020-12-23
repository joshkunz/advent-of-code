use std::{result, env, fs};
use std::str::FromStr;
use std::collections::HashMap;

/*
dotted salmon bags contain 2 dark lavender bags, 1 muted red bag, 1 vibrant magenta bag.
vibrant purple bags contain 1 pale cyan bag, 1 dotted lavender bag, 3 striped blue bags, 5 clear magenta bags.
vibrant fuchsia bags contain 4 posh violet bags, 3 bright aqua bags, 1 light silver bag.
mirrored purple bags contain 2 dim yellow bags, 4 dim green bags, 3 vibrant beige bags.
faded coral bags contain 1 vibrant plum bag, 3 pale gold bags, 5 dim purple bags, 1 drab teal bag.
wavy cyan bags contain 4 dark teal bags, 1 dotted magenta bag.
dotted gold bags contain 3 dotted gray bags.
shiny maroon bags contain 2 light white bags, 5 bright salmon bags.
vibrant cyan bags contain 2 dull beige bags.
clear fuchsia bags contain 5 bright bronze bags.
*/

#[derive(Debug,PartialEq,Eq)]
struct Error(String);

impl<T: ToString> From<T> for Error {
    fn from(v: T) -> Self {
        return Error(v.to_string());
    }
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug,PartialEq,Eq,Hash)]
struct Bag(String);

impl<T: ToString> From<T> for Bag {
    fn from(v: T) -> Self {
        return Bag(v.to_string());
    }
}

#[derive(Debug,PartialEq)]
struct Clause {
    count: i64,
    bag: Bag,
}

impl FromStr for Clause {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let no_suffix = s
            .strip_suffix("bags")
            .or_else(|| s.strip_suffix("bag"))
            .map(str::trim)
            .ok_or("No bags suffix")?;
        let parts: Vec<&str> = no_suffix.splitn(2, " ").collect();
        assert_eq!(parts.len(), 2);
        return Ok(Clause{
            count: i64::from_str(&parts[0])?,
            bag: Bag::from(&parts[1]),
        });
    }
}

#[derive(Debug,PartialEq)]
struct Rule {
    bag: Bag,
    clauses: Vec<Clause>,
}

impl FromStr for Rule {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let parts: Vec<&str> = s.split("bags contain").collect();
        assert_eq!(parts.len(), 2);
        let bag = Bag::from(parts[0].trim());

        if parts[1].trim() == "no other bags." {
            return Ok(Rule{
                bag: bag,
                clauses: Vec::new(),
            });
        }
        let clauses: Vec<Clause> = parts[1]
            .strip_suffix(".")
            .unwrap()
            .split(",")
            .map(str::trim)
            .map(|s| s.parse().unwrap())
            .collect();
        return Ok(Rule{
            bag: bag,
            clauses: clauses,
        });
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let rules: Vec<Rule> = input_raw
        .lines()
        .map(|l| Rule::from_str(l).unwrap())
        .collect();
    
    let rmap: HashMap<&Bag, &Rule> = rules.iter()
        .map(|r| (&r.bag, r))
        .collect();

    let target = Bag::from("shiny gold"); 

    let mut count: i64 = 0;
    for r in &rules {
        let mut next = vec![&r.bag];
        'search: while !next.is_empty() {
            let cur = next.remove(0);
            for c in &rmap[cur].clauses {
                if c.bag == target {
                    count += 1;
                    break 'search;
                }
                next.push(&c.bag);
            }
        }
    }

    println!("Count: {}", count);

}
