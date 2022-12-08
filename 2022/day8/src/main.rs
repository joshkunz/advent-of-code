use std::io::Read;
use std::io;

fn main() {
    let mut input: String = String::default();
    io::stdin().read_to_string(&mut input).unwrap();

    println!("Part 1: {}", solve_part1(&input));
    println!("Part 2: {}", solve_part2(&input));
}

fn solve_part1(input: &str) -> i64 {
    let puzzle = parse(input);
    return nspots(&puzzle) - (hidden(&puzzle).len() as i64);
}

fn solve_part2(input: &str) -> i64 {
    let puzzle = parse(input);
    let rows = puzzle.len();
    let cols = puzzle[0].len();
    let scn = Sceneic::new(&puzzle);

    let mut max_score: i64 = 0;

    for row in 1..rows-1 {
        for col in 1..cols-1 {
            let height = puzzle[row][col] as usize;
            let score = 
                (scn.by_height[height].left[row][col-1] as i64)
                * (scn.by_height[height].right[row][col+1] as i64)
                * (scn.by_height[height].up[row-1][col] as i64)
                * (scn.by_height[height].down[row+1][col] as i64);
            if score > max_score {
                max_score = score;
            }
        }
    }

    return max_score;
}

fn nspots(puzzle: &Vec<Vec<u8>>) -> i64 {
    return (puzzle.len() * puzzle[0].len()) as i64;
}

fn parse(input: &str) -> Vec<Vec<u8>> {
    let mut out: Vec<Vec<u8>> = Vec::new();
    for l in input.lines().map(|l| l.trim()) {
        let row: Vec<u8> = l.bytes().map(|c| c - ('0' as u8)).collect();
        out.push(row);
    }
    return out;
}

fn hidden(puzzle: &Vec<Vec<u8>>) -> Vec<(i64, i64)> {
    let maxes = Maxes::new(puzzle);
    let rows = puzzle.len();
    let cols = puzzle[0].len();

    let mut out: Vec<(i64, i64)> = Vec::new();

    // 1..n-1 because the top/bottom/leftmost/rightmost cannot be hidden.
    for row in 1..rows-1 {
        for col in 1..cols-1 {
            let val = puzzle[row][col];
            let hidden = 
                maxes.0.left[row][col-1] >= val
                && maxes.0.right[row][col+1] >= val
                && maxes.0.up[row-1][col] >= val
                && maxes.0.down[row+1][col] >= val;
            if hidden {
                out.push((row as i64, col as i64));
            }
        }
    }
    return out;
}

#[derive(Default, PartialEq, Eq, Debug)]
struct Directional {
    left: Vec<Vec<u8>>,
    right: Vec<Vec<u8>>,
    up: Vec<Vec<u8>>,
    down: Vec<Vec<u8>>,
}

impl Directional {
    fn new(rows: usize, cols: usize) -> Self {
        let mut ret = Directional::default();
        for _ in 0..rows {
            ret.left.push(Vec::new());
            ret.right.push(Vec::new());
        }

        for _ in 0..cols {
            ret.up.push(Vec::new());
            ret.down.push(Vec::new());
        }
        return ret;
    }
}

#[derive(Default, PartialEq, Eq, Debug)]
struct Sceneic {
    by_height: Vec<Directional>,
}

impl Sceneic {
    fn new(puzzle: &Vec<Vec<u8>>) -> Self{
        let rows = puzzle.len();
        let cols = puzzle[0].len();

        let mut ret = Sceneic::default();
        for height in 0..=9 {
            let mut dir = Directional::new(rows, cols);

            for (idx, row) in puzzle.iter().enumerate() {
                let left = &mut dir.left[idx];
                let right = &mut dir.right[idx];

                let mut count_right: u8 = 0;
                let mut count_left: u8 = 0;

                for col in 0..cols {
                    if row[col] >= height {
                        count_left = 1;
                    } else {
                        count_left+=1;
                    }
                    left.push(count_left);

                    if row[cols-col-1] >= height {
                        count_right = 1;
                    } else {
                        count_right+=1
                    }
                    right.push(count_right);
                }
                right.reverse();
            }

            for col in 0..cols {
                let mut count_up: u8 = 0;
                let mut count_down: u8 = 0;

                for row in 0..rows {
                    let up = &mut dir.up[row];
                    // We actually want to build this bottom-up.
                    let down = &mut dir.down[rows-row-1];

                    if puzzle[row][col] >= height {
                        count_up = 1;
                    } else {
                        count_up+=1;
                    }
                    up.push(count_up);

                    if puzzle[rows-row-1][col] >= height {
                        count_down = 1;
                    } else {
                        count_down+=1;
                    }
                    down.push(count_down);
                }
            }

            ret.by_height.push(dir);
        }

        return ret;
    }
}

#[derive(Default, PartialEq, Eq, Debug)]
struct Maxes(Directional);

impl Maxes {
    fn new(puzzle: &Vec<Vec<u8>>) -> Self {
        let rows = puzzle.len();
        let cols = puzzle[0].len();
        let mut ret = Directional::new(rows, cols);

        for (idx, row) in puzzle.iter().enumerate() {
            let left = &mut ret.left[idx];
            let right = &mut ret.right[idx];

            let mut max_right = *row.last().unwrap();
            let mut max_left = *row.first().unwrap();

            for col in 0..cols {
                if row[col] > max_left {
                    max_left = row[col];
                }
                left.push(max_left);

                if row[cols-col-1] > max_right {
                    max_right = row[cols-col-1];
                }
                right.push(max_right);
            }
            right.reverse();
        }

        for col in 0..cols {
            let mut max_up: u8 = 0;
            let mut max_down: u8 = 0;

            for row in 0..rows {
                let up = &mut ret.up[row];
                // We actually want to build this bottom-up.
                let down = &mut ret.down[rows-row-1];

                if puzzle[row][col] > max_up {
                    max_up = puzzle[row][col];
                }
                up.push(max_up);

                if puzzle[rows-row-1][col] > max_down {
                    max_down = puzzle[rows-row-1][col];
                }
                down.push(max_down);
            }
        }

        return Maxes(ret);
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let test = "
30373
25512
65332
33549
35390
".trim();
        let want: Vec<Vec<u8>> = vec![
            vec![3, 0, 3, 7, 3],
            vec![2, 5, 5, 1, 2],
            vec![6, 5, 3, 3, 2],
            vec![3, 3, 5, 4, 9],
            vec![3, 5, 3, 9, 0],
        ];

        assert_eq!(parse(test), want);
    }

    #[test]
    fn test_maxes_new() {
        let puzzle: Vec<Vec<u8>> = vec![
            vec![3, 0, 3, 7, 3],
            vec![2, 5, 5, 1, 2],
            vec![6, 5, 3, 3, 2],
            vec![3, 3, 5, 4, 9],
            vec![3, 5, 3, 9, 0],
        ];

        let want = Maxes(Directional{
            left: vec![
                vec![3, 3, 3, 7, 7],
                vec![2, 5, 5, 5, 5],
                vec![6, 6, 6, 6, 6],
                vec![3, 3, 5, 5, 9],
                vec![3, 5, 5, 9, 9],
            ],
            right: vec![
                vec![7, 7, 7, 7, 3],
                vec![5, 5, 5, 2, 2],
                vec![6, 5, 3, 3, 2],
                vec![9, 9, 9, 9, 9],
                vec![9, 9, 9, 9, 0],
            ],
            up: vec![
                vec![3, 0, 3, 7, 3],
                vec![3, 5, 5, 7, 3],
                vec![6, 5, 5, 7, 3],
                vec![6, 5, 5, 7, 9],
                vec![6, 5, 5, 9, 9],
            ],
            down: vec![
                vec![6, 5, 5, 9, 9],
                vec![6, 5, 5, 9, 9],
                vec![6, 5, 5, 9, 9],
                vec![3, 5, 5, 9, 9],
                vec![3, 5, 3, 9, 0],
            ],
        });

        let got = Maxes::new(&puzzle);
        assert_eq!(got, want);
    }

    #[test]
    fn test_hidden() {
        let puzzle: Vec<Vec<u8>> = vec![
            vec![3, 0, 3, 7, 3],
            vec![2, 5, 5, 1, 2],
            vec![6, 5, 3, 3, 2],
            vec![3, 3, 5, 4, 9],
            vec![3, 5, 3, 9, 0],
        ];
        let want: Vec<(i64, i64)> = vec![
            (1, 3),
            (2, 2),
            (3, 1),
            (3, 3),
        ];

        let got = hidden(&puzzle);
        assert_eq!(want, got);
    }
}
