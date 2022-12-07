use anyhow::*;
use std::collections::HashMap;
use std::io::Read;
use std::io;

fn main() {
    let mut input: String = String::default();
    io::stdin().read_to_string(&mut input).unwrap();

    println!("Part 1: {}", part1(&input).expect("part 1"));
    println!("Part 2: {}", part2(&input).expect("part 2"));
}

fn part1(input: &str) -> Result<usize> {
    let fs = parse(input)?;

    return Ok(fs.dir_sizes().into_iter().map(|(_, size)| {
        if size < 100000 {
            size
        } else {
            0
        }
    }).sum());
}

fn part2(input: &str) -> Result<usize> {
    let fs = parse(input)?;

    let total: usize = 70000000;
    let need_free: usize = 30000000;
    let currently_free = total - fs.dir_size("/");

    return fs.dir_sizes().into_iter().filter_map(|(_, size)| {
        if size >= need_free - currently_free {
            Some(size)
        } else {
            // Filter out directories that can't satisfy the requirement.
            None
        }
    }).min().ok_or(anyhow!("could not find candidate"));
}

#[derive(Debug, PartialEq, Eq)]
struct FileEntry {
    name: String,
    size: usize
}

#[derive(Debug, PartialEq, Eq)]
enum Entry {
    Dir(String),
    File(FileEntry)
}

struct Solver<'a> {
    memo: HashMap<&'a str, usize>,
    fs: &'a FS
}

impl<'a> Solver<'a> {
    fn new(fs: &'a FS) -> Self {
        return Solver{
            memo: HashMap::new(),
            fs: fs,
        };
    }

    fn size_of(&mut self, path: &'a str) -> usize {
        if self.memo.contains_key(path) {
            return self.memo[path];
        }
        let size: usize = self.fs.tree[path].iter().map(|v| {
            match v {
                Entry::Dir(p) => self.size_of(p),
                Entry::File(FileEntry{size, ..}) => *size,
            }
        }).sum();
        self.memo.insert(path, size);
        return size;
    }
}

#[derive(Default,Debug,PartialEq,Eq)]
struct FS {
    tree: HashMap<String, Vec<Entry>>,
}

impl FS {
    fn add_entry(&mut self, path: &str, e: Entry) {
        let real_path: &str = if path == "" {
            "/"
        } else {
            path
        };
        if !self.tree.contains_key(real_path) {
            self.tree.insert(String::from(real_path), Vec::new());
        }
        self.tree.get_mut(real_path).unwrap().push(e);
    }

    fn dir_size(&self, path: &str) -> usize {
        let mut solver = Solver::new(self);
        return solver.size_of(path);
    }

    fn dir_sizes(&self) -> Vec<(String, usize)> {
        let mut solver = Solver::new(self);
        return self.tree.keys().map(|p| {
            (p.clone(), solver.size_of(p))
        }).collect();
    }
}

fn parse(input: &str) -> Result<FS> {
    let lines: Vec<&str> = input.split("\n").collect();

    let mut path: Vec<&str> = vec![""];
    let mut result: FS = FS::default();

    for l in lines.iter() {
        let parts: Vec<&str> = l.split_whitespace().collect();
        match parts[..] {
            ["$", "cd", ".."] => {
                path.pop();
            },
            ["$", "cd", "/"] => {
                path = vec![""];
            },
            ["$", "cd", into_dir] => {
                path.push(into_dir);
            },
            ["$", ..] => {
                // Just ignore other commands.
            },
            ["dir", dir_name] => {
                let path = path.join("/");
                let dir = Entry::Dir([&path, dir_name].join("/").to_string());
                result.add_entry(&path, dir);
            },
            [size, fname] => {
                let path = path.join("/");
                let file = Entry::File(FileEntry{
                    name: fname.to_string(),
                    size: size.parse()?,
                });
                result.add_entry(&path, file);
            },
            [..] => {
                bail!("cannot parse parts {:#?}, line: {:#?}", parts, l);
            }
        }
    }

    return Ok(result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "
$ cd down
$ ls
dir foo
dir bar
12345 a.txt
$ cd ..
$ ls
dir down
54321 b.txt
".trim();
        let mut want: FS = FS::default();
        want.add_entry("/down", Entry::Dir("/down/foo".to_string()));
        want.add_entry("/down", Entry::Dir("/down/bar".to_string()));
        want.add_entry("/down", Entry::File(FileEntry{size: 12345, name: "a.txt".to_string()}));
        want.add_entry("/", Entry::Dir("/down".to_string()));
        want.add_entry("/", Entry::File(FileEntry{size: 54321, name: "b.txt".to_string()}));

        let got = parse(input);
        assert!(got.is_ok());
        assert_eq!(got.unwrap(), want);
    }

    #[test]
    fn test_dir_sizes() {
        let mut d: FS = FS::default();
        d.add_entry("/down", Entry::File(FileEntry{size: 12345, name: "a.txt".to_string()}));
        d.add_entry("/", Entry::Dir("/down".to_string()));
        d.add_entry("/", Entry::File(FileEntry{size: 54321, name: "b.txt".to_string()}));

        let mut want: Vec<(String, usize)> = vec![
            ("/".to_string(), 12345 + 54321),
            ("/down".to_string(), 12345),
        ];

        let mut got = d.dir_sizes();

        want.sort();
        got.sort();

        assert_eq!(got, want);
    }
}
