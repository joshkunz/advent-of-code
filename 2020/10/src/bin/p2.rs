use std::collections::{HashMap, HashSet};
use std::str::FromStr;
use std::hash::Hash;
use std::fmt::Debug;
use std::{env, fs};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
struct Adapter(i64);

impl Adapter {
    fn output(&self) -> i64 {
        let Adapter(v) = self;
        return *v;
    }
}

impl From<i64> for Adapter {
    fn from(v: i64) -> Self {
        Adapter(v)
    }
}

#[derive(Debug)]
struct Graph<T: Eq + Hash> {
    vmap: HashMap<T, HashSet<T>>,
}

impl<T: Eq + Hash + Debug> Graph<T> {
    fn new() -> Self {
        Graph {
            vmap: HashMap::new(),
        }
    }

    fn push_edge(&mut self, from: T, to: T) {
        if !self.vmap.contains_key(&from) {
            self.vmap.insert(from, vec![to].into_iter().collect());
        } else {
            let vs = self.vmap.get_mut(&from).unwrap();
            vs.insert(to);
        }
    }

    fn neighbors(&self, node: &'_ T) -> Vec<&T> {
        let mut out: Vec<&T> = Vec::new();
        if !self.vmap.contains_key(node) {
            return out;
        }
        for f in &self.vmap[node] {
            out.push(f);
        }
        return out;
    }

    fn is_terminal(&self, node: &'_ T) -> bool {
        if !self.vmap.contains_key(node) {
            return true;
        }
        return self.vmap[node].is_empty();
    }
}

fn adapter_graph(mut aes: Vec<Adapter>) -> Graph<Adapter> {
    let mut g: Graph<Adapter> = Graph::new();

    let max: Adapter = *aes.iter().max().unwrap();
    // Push in the source node.
    aes.push(Adapter::from(0));
    // Push in the sink node.
    aes.push(Adapter::from(max.output() + 3));

    aes.sort();

    for idx in 0..aes.len() {
        let cur = &aes[idx];
        for to in &aes[idx + 1..] {
            if to.output() > (cur.output() + 3) {
                break;
            }
            g.push_edge(*cur, *to);
        }
    }

    return g;
}

struct Solver<'a> {
    g: &'a Graph<Adapter>,
    cache: HashMap<&'a Adapter, i64>
}

impl<'a> Solver<'a> {
    fn new(g: &'a Graph<Adapter>) -> Self {
        Solver{
            g: g,
            cache: HashMap::new(),
        }
    }

    fn solve(&mut self, start: &'a Adapter) -> i64 {
        if self.cache.contains_key(start) {
            return self.cache[start];
        }
        if self.g.is_terminal(start) {
            self.cache.insert(start, 1);
            return 1;
        }
        let mut res: i64 = 0;
        for to in self.g.neighbors(start) {
            res += self.solve(to);
        }
        self.cache.insert(start, res);
        return res;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input_raw = fs::read_to_string(&args[1]).expect("argument must be a valid string");
    let input: Vec<Adapter> = input_raw
        .lines()
        .map(|l| Adapter::from(i64::from_str(l.trim()).unwrap()))
        .collect();

    let g = adapter_graph(input);
    let soln = Solver::new(&g).solve(&Adapter::from(0));

    println!("Solution: {}", soln);
}
