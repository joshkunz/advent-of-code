use regex::Regex;
use std::collections::HashMap;
use anyhow::Result;
use std::io;
use std::io::Read;

fn main() {
    let mut input: String = String::default();
    io::stdin().read_to_string(&mut input).unwrap();

    println!("solution: {}", solve_part1(&input));
    println!("solution: {}", solve_part2(&input));
}

fn top_containers(containers: HashMap<String, Vec<char>>) -> String {
    let mut ks: Vec<&String> = containers.keys().collect();
    ks.sort();
    let mut solution: String = String::default();

    for k in ks {
        solution.push(*containers[k].last().unwrap());
    }
    return solution;
}

struct Move{
    amount: usize,
    from: String,
    to: String,
}

fn moves(input: &str) -> Vec<Move> {
    let move_re = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();

    // Skip past the first blank line (including the blank line itself)
    return input.split("\n").skip_while(|&s| s != "").skip(1).map(|l| {
        let m = move_re.captures(l).unwrap();

        Move{
            amount: m.get(1).unwrap().as_str().parse().unwrap(),
            from: m.get(2).unwrap().as_str().to_string(),
            to: m.get(3).unwrap().as_str().to_string(),
        }
    }).collect();
}

fn solve_part2(input: &str) -> String {
    let mut containers = parse_initial(&input).expect("failed to parse");

    for Move{amount, from, to} in moves(input) {
        let src_len = containers[&from].len();

        let mut to_move: Vec<char> = containers
            .get_mut(&from)
            .unwrap()
            .split_off(src_len-amount);
        containers
            .get_mut(&to)
            .unwrap()
            .append(&mut to_move);
    }

    return top_containers(containers);
}

fn solve_part1(input: &str) -> String {
    let mut containers = parse_initial(&input).expect("failed to parse");

    for Move{amount, from, to} in moves(input) {
        for _i in 0..amount {
            let chr: char = containers
                .get_mut(&from)
                .unwrap()
                .pop()
                .unwrap();
            containers
                .get_mut(&to)
                .unwrap()
                .push(chr);
        }
    }

    return top_containers(containers);
}

fn parse_initial(from: &str) -> Result<HashMap<String, Vec<char>>> {
    let container_re = Regex::new(r"\[([A-Z])\]")?;
    let position_re = Regex::new(r"\d+")?;

    let mut containers: Vec<(i16, char)> = Vec::new();
    let mut positions: Vec<(i16, String)> = Vec::new();
    for l in from.split("\n") {
        let mut matched = false;
        for cs in container_re.captures_iter(l) {
            matched = true;

            let m = cs.get(1).unwrap();
            assert!(m.start() == m.end()-1);
            let container = m.as_str().chars().next().unwrap();
            containers.push((m.start() as i16, container));
        }
        if matched {
            continue;
        }

        // If we get here, then we are on the container label level, so match
        // the positions.
        for m in position_re.find_iter(l) {
            positions.push((m.start() as i16, String::from(m.as_str())));
        }
        break;
    }

    let mut out: HashMap<String, Vec<char>> = HashMap::new();
    for (_, name) in positions.iter() {
        out.insert(name.clone(), Vec::new());
    }
    let position_map: HashMap<i16, String> = HashMap::from_iter(positions);

    for (position, id) in containers.iter().rev() {
        out.get_mut(&position_map[&position]).unwrap().push(*id);
    }

    return Ok(out);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_initial() {
        let want = vec![
            ("1".to_string(), vec!['D', 'B', 'A']),
            ("2".to_string(), vec!['E', 'C']),
            ("3".to_string(), vec!['F']),
        ];
        let out = parse_initial(" [A]
 [B] [C]
 [D] [E] [F]
  1   2   3
");
        assert!(out.is_ok());
        assert_eq!(out.unwrap().into_iter().collect::<Vec<(String,Vec<char>)>>(), want);
    }
}
