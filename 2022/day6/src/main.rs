use std::io;
use std::io::Read;

fn main() {
    let mut input: String = String::default();
    io::stdin().read_to_string(&mut input).unwrap();

    println!("Part 1: {}", packet_start(&input, 4).expect("failed to find start"));
    println!("Part 2: {}", packet_start(&input, 14).expect("failed to find start"));
}

fn packet_start(input: &str, length: usize) -> Option<usize> {
    let bs: Vec<u8> = input.bytes().collect();

    for idx in (length-1)..bs.len() {
        let mut is_start: bool = true;
        'check: for first in 0..length {
            for second in (first+1)..length {
                is_start = is_start && bs[idx-first] != bs[idx-second];
                if !is_start {
                    break 'check;
                }
            }
        }

        if is_start {
            // +1 because we're talking about the ith character here, not the
            // index.
            return Some(idx+1);
        }
    }

    return None;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_packet_start_part1() {
        assert_eq!(packet_start("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), Some(7));
        assert_eq!(packet_start("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), Some(5));
        assert_eq!(packet_start("nppdvjthqldpwncqszvftbrmjlhg", 4), Some(6));
        assert_eq!(packet_start("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4), Some(10));
        assert_eq!(packet_start("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), Some(11));
    }

    #[test]
    fn test_packet_start_part2() {
        assert_eq!(packet_start("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), Some(19));
        assert_eq!(packet_start("bvwbjplbgvbhsrlpgdmjqwftvncz", 14), Some(23));
        assert_eq!(packet_start("nppdvjthqldpwncqszvftbrmjlhg", 14), Some(23));
        assert_eq!(packet_start("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14), Some(29));
        assert_eq!(packet_start("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14), Some(26));
    }
}
