use std::str::FromStr;
use std::num::ParseIntError;
use std::io;
use std::io::BufRead;
use std::borrow::Borrow;

#[derive(Debug,PartialEq,Eq)]
struct IntRange {
    start: i64,
    end: i64,
}

impl IntRange {
    fn contains(&self, other: &IntRange) -> bool {
        return other.start >= self.start && other.end <= self.end;
    }

    fn within(&self, point: i64) -> bool {
        return self.start <= point && self.end >= point;
    }

    fn overlaps(&self, other: &IntRange) -> bool {
        return 
            self.within(other.start)
            || self.within(other.end)
            || other.contains(self);
    }

    fn new(start: i64, end: i64) -> Self {
        return Self{start: start, end: end};
    }
}

impl FromStr for IntRange {
    type Err = String;

    fn from_str(v: &str) -> Result<Self, String> {
        let parts: Vec<&str> = v.split("-").collect();
        if parts.len() != 2 {
            return Err(String::from("invalid range"));
        }
        return Ok(IntRange{
            start: parts[0].parse().map_err(|v: ParseIntError| v.to_string())?,
            end: parts[1].parse().map_err(|v: ParseIntError| v.to_string())?,
        })
    }
}

fn parse<V: Borrow<str> , I: Iterator<Item = V>>(input: I) -> Result<Vec<(IntRange, IntRange)>, String> {
    let out: Vec<(IntRange, IntRange)> = input.map(|l| {
        let parts: Vec<&str> = l.borrow().split(",").collect();
        if parts.len() != 2 {
            let input: &str = l.borrow();
            return Err(format!("wrong number of parts in {input}"));
        }
        let a: IntRange = parts[0].parse()?;
        let b: IntRange = parts[1].parse()?;
        return Ok((a, b));
    }).collect::<Result<Vec<(IntRange, IntRange)>, String>>()?;
    return Ok(out);
}

fn solve_part1<'a, I: Iterator<Item = &'a (IntRange, IntRange)>>(input: I) -> i64 {
    input.map(|(a, b)| {
        if a.contains(&b) || b.contains(&a) { 1 } else { 0 }
    }).sum()
}

fn solve_part2<'a, I: Iterator<Item = &'a (IntRange, IntRange)>>(input: I) -> i64 {
    input.map(|(a, b)| {
        if a.overlaps(&b) { 1 } else { 0 }
    }).sum()
}

fn main() {
    //let input: Vec<String> = io::stdin().lock().collect).expect("failed to read");
    let input: Vec<String> = io::stdin()
        .lock()
        .lines()
        .collect::<Result<Vec<String>, io::Error>>()
        .expect("failed to read");
    let puzzle = parse(input.into_iter()).expect("failed to parse");
    let solution_p1 = solve_part1(puzzle.iter());
    let solution_p2 = solve_part2(puzzle.iter());
    println!("Part1: {solution_p1}");
    println!("Part2: {solution_p2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intrange_fromstr() {
        assert_eq!("1-2".parse(), Ok(IntRange{start:1, end:2}));
        assert!("1-".parse::<IntRange>().is_err());
    }

    #[test]
    fn test_intrange_contains() {
        assert!(IntRange{start:1, end:10}.contains(&IntRange{start:2, end:5}));
        assert!(IntRange{start:3, end:7}.contains(&IntRange{start:3, end:7}));
        assert!(!IntRange{start:1, end:5}.contains(&IntRange{start:4, end:10}));
    }

    #[test]
    fn test_parse() {
        let input = vec![
            "1-2,3-4",
            "5-6,7-8",
        ];
        assert_eq!(
            parse(input.into_iter()),
            Ok(vec![
                (IntRange{start:1, end:2}, IntRange{start:3, end:4}),
                (IntRange{start:5, end:6}, IntRange{start:7, end:8}),
            ]),
        );

        let bad = vec![
            "1-2,",
        ];
        assert!(parse(bad.into_iter()).is_err());
    } 

    #[test]
    fn test_solve_part1() {
        let input = vec![
            (IntRange{start: 1, end: 5}, IntRange{start:2, end:5}),
            (IntRange{start: 1, end: 5}, IntRange{start:10, end:15}),
        ];
        assert_eq!(solve_part1(input.iter()), 1);
    }

    #[test]
    fn test_overlaps() {
        let cases = vec![
            // End overlaps
            (IntRange::new(1, 3), IntRange::new(-2, 2)),
            // Start overlaps
            (IntRange::new(1, 3), IntRange::new(2, 5)),
            // a contains b
            (IntRange::new(1, 15), IntRange::new(2, 5)),
            // b contains a
            (IntRange::new(3, 5), IntRange::new(1, 10)),
            // End overlap 1
            (IntRange::new(1, 2), IntRange::new(2, 3)),
            // Start overlap 1
            (IntRange::new(1, 2), IntRange::new(0, 1)),
        ];

        for case in cases {
            assert!(case.0.overlaps(&case.1));
            // Should work in both directions
            assert!(case.1.overlaps(&case.0));
        }
    }

}
