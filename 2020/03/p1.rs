use std::{result, env, fs};
use std::ops::Index;

type Result<T> = result::Result<T, String>;

#[derive(PartialEq,Clone,Copy,Debug)]
enum Tile {
    Empty,
    Tree,
}

struct Map {
    left_tiles: Vec<Tile>,
    rows: usize,
    cols: usize,
}

impl Map {
    fn parse(s: &str) -> Result<Self> {
        let mut tiles: Vec<Tile> = Vec::new();

        let mut rows: usize = 0;
        for l in s.lines() {
            rows += 1;
            for c in l.chars() {
                match c {
                    '.' => tiles.push(Tile::Empty),
                    '#' => tiles.push(Tile::Tree),
                    _ => return Err(format!("unrecognized character {}", c)),
                }
            }
        }

        // total = rows * cols, so cols = total / rows
        let cols = tiles.len() / rows;

        return Ok(Map{
            left_tiles: tiles,
            rows: rows,
            cols: cols,
        });
    }

    // Origin is upper-left at 0, 0.
    fn at(&self, x: usize, y: usize) -> Result<&Tile> {
        if y > self.rows {
            return Err(format!("{} is out of bounds (max: {})", y, self.rows));
        }

        let idx = (y * self.cols) + (x % self.cols);
        return Ok(&self.left_tiles[idx]);
    }
}

impl Index<(usize, usize)> for Map {
    type Output = Tile;

    fn index(&self, idx: (usize, usize)) -> &Self::Output {
        return self.at(idx.0, idx.1).unwrap();
    }
}

struct Slope {
    right: usize,
    down: usize,
}

fn solve(m: &Map, slope: &Slope) -> i64 {
    let mut x: usize = 0;
    let mut y: usize = 0;

    let mut trees: i64 = 0;
    while m.rows > y {
        if m[(x, y)] == Tile::Tree {
            trees += 1;
        }
        x += slope.right;
        y += slope.down;
    }
    return trees;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let map = Map::parse(&input_raw).unwrap();

    let slope = Slope{right: 3, down: 1};

    println!("Trees! {}", solve(&map, &slope));
}
