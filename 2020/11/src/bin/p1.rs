use std::{result, env, fs, fmt};
use std::str::FromStr;
use core::ops::Index;

type Error = String;
type Result<T> = result::Result<T, Error>;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Tile {
    Floor,
    Empty,
    Occupied,
}

impl FromStr for Tile {
    type Err = Error;

    fn from_str(s: &str) -> Result<Tile> {
        match s {
            "." => Ok(Tile::Floor),
            "L" => Ok(Tile::Empty),
            "#" => Ok(Tile::Occupied),
            _ => Err(format!("Unrecognized tile '{}'", s)),
        }
    }
}

impl ToString for Tile {
    fn to_string(&self) -> String {
        String::from(match self {
            Tile::Floor => ".",
            Tile::Empty => "L",
            Tile::Occupied => "#",
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Room {
    tiles: Vec<Tile>,
    rows: usize,
    cols: usize,
}

impl Index<(usize, usize)> for Room {
    type Output = Tile;

    fn index(&self, index: (usize, usize)) -> &Tile {
        let (x, y) = index;
        let linear_idx = (y * self.cols) + x;
        return &self.tiles[linear_idx];
    }
}

impl Room {
    fn at(&self, x: isize, y: isize) -> Option<&Tile> {
        if x < 0 || y < 0 || x >= (self.cols as isize) || y >= (self.rows as isize) {
            return None;
        }

        return Some(&self[((x as usize), (y as usize))]);
    }

    fn adj(&self, x: usize, y: usize) -> Vec<&Tile> {
        /*  
         *  V V V
         *  V C V
         *  V V V
         */
        let x = x as isize;
        let y = y as isize;
        return vec![
            // Top line.
            self.at(x-1, y-1),
            self.at(x, y-1),
            self.at(x+1, y-1),

            // Center line.
            self.at(x-1, y),
            self.at(x+1, y),

            // Bottom line.
            self.at(x-1, y+1),
            self.at(x, y+1),
            self.at(x+1, y+1),
        ].into_iter().flatten().collect();
    }

    fn next_at(&self, x: usize, y: usize) -> Tile {
        let cur = self[(x, y)];
        let adj: Vec<&Tile> = self.adj(x, y);

        let n_occupied = adj
            .into_iter()
            .filter(|v| **v == Tile::Occupied)
            .count();
        match cur {
            Tile::Empty if n_occupied == 0 => Tile::Occupied,
            Tile::Occupied if n_occupied >= 4 => Tile::Empty,
            v => v,
        }
    }

    fn step(&self) -> Room {
        let mut tiles: Vec<Tile> = Vec::new();
        for j in 0..self.rows {
            for i in 0..self.cols {
                tiles.push(self.next_at(i, j));
            }
        }
        return Room{
            tiles: tiles,
            rows: self.rows,
            cols: self.cols,
        };
    }

    fn n_occupied(&self) -> i64 {
        self.tiles
            .iter()
            .filter(|v| **v == Tile::Occupied)
            .count() as i64
    }
}

impl FromStr for Room {
    type Err = Error;

    fn from_str(s: &str) -> Result<Room> {
        let mut tiles: Vec<Tile> = Vec::new();
        let mut rows: usize = 0; 

        for row in s.lines() {
            rows += 1;
            for c in row.chars() {
                tiles.push(Tile::from_str(&c.to_string())?);
            }
        }

        let cols = tiles.len() / rows;
        return Ok(Room{
            tiles: tiles,
            rows: rows,
            cols: cols,
        });
    }
}

impl fmt::Display for Room {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> result::Result<(), fmt::Error> {
        for r in 0..self.rows {
            let mut row = String::with_capacity(self.cols + 1);
            for c in 0..self.cols {
                row.push_str(&self[(c, r)].to_string());
            }
            row.push('\n');
            f.write_str(&row)?;
        }
        Ok(())
    }
}

fn fix(init: Room) -> Room {
    let mut prev = init;
    let mut cur = prev.step();

    while prev != cur {
        prev = cur;
        cur = prev.step();
    }

    return cur;
}

fn main() {
    assert_eq!(Tile::from_str("."), Ok(Tile::Floor));
    assert_eq!(Tile::from_str("L"), Ok(Tile::Empty));
    assert_eq!(Tile::from_str("#"), Ok(Tile::Occupied));

    assert_eq!(Room::from_str("LL.#L\n..#L."), Ok(Room{
        tiles: vec![
            Tile::Empty, Tile::Empty, Tile::Floor, Tile::Occupied, Tile::Empty,
            Tile::Floor, Tile::Floor, Tile::Occupied, Tile::Empty, Tile::Floor,
        ],
        rows: 2,
        cols: 5,
    }));
    assert_eq!(
        Room::from_str("LL.#L\n..#L.").unwrap().adj(0, 0),
        vec![
            &Tile::Empty,
            &Tile::Floor,
            &Tile::Floor,
        ],
    );

    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");

    let input: Room = input_raw.parse().unwrap();

    println!("{}", input);
    println!("{}", input.step());

    let fin = fix(input);
    println!("Final:\n{}", fin);
    println!("N Occupied: {}", fin.n_occupied());
}
