use num_traits::{Zero, One};
use std::collections::HashMap;
use arrayvec::ArrayVec;
use fasthash::xx::Hash64;

type Rat = num_rational::Ratio<u64>;

const ADD: u8 = ::std::u8::MAX;
const SUB: u8 = ::std::u8::MAX - 1;
const MUL: u8 = ::std::u8::MAX - 2;
const DIV: u8 = ::std::u8::MAX - 3;

type Op = u8;

#[derive(Clone)]
struct Expr
{
    ops: Vec<Op>,
    val: Rat
}

impl Expr
{
    fn new(nrs: &[u64], idx: u8) -> Self
    {
        Expr { ops: vec![idx], val: Rat::from_integer(nrs[idx as usize]) }
    }

    fn possible_combinations(&self, expr: &Self) -> ArrayVec<[(char, Rat); 6]>
    {
        let mut res = ArrayVec::<[_; 6]>::new();

        let op0 = *self.ops.last().unwrap();
        let op1 = *expr.ops.last().unwrap();
        if op1 != ADD && op1 != SUB
        {
            if op0 != SUB
            {
                res.push(('+', self.val + expr.val));
            }
            if self.val >= expr.val && !expr.val.is_zero()
            {
                res.push(('-', self.val - expr.val));
            }
        }

        if op1 != MUL && op1 != DIV
        {
            if op0 != DIV
            {
                res.push(('*', self.val * expr.val));
            }
            if !expr.val.is_zero() && !expr.val.is_one()
            {
                res.push(('/', self.val / expr.val));
            }
        }

        if op0 != ADD && op0 != SUB && expr.val >= self.val && !self.val.is_zero()
        {
            res.push(('_', expr.val - self.val));
        }
        if op0 != MUL && op0 != DIV && !self.val.is_zero() && !self.val.is_one()
        {
            res.push(('\\', expr.val / self.val));
        }

        res
    }

    fn combine(&self, expr: &Self, op: char, val: Rat) -> Self
    {
        let mut ops;
        match op
        {
            '+' => {
                ops = [&self.ops[..], &expr.ops[..]].concat();
                ops.push(ADD);
            },
            '-' => {
                ops = [&self.ops[..], &expr.ops[..]].concat();
                ops.push(SUB);
            },
            '*' => {
                ops = [&self.ops[..], &expr.ops[..]].concat();
                ops.push(MUL);
            },
            '/' => {
                ops = [&self.ops[..], &expr.ops[..]].concat();
                ops.push(DIV);
            },
            '_' => {
                ops = [&expr.ops[..], &self.ops[..]].concat();
                ops.push(SUB);
            },
            '\\' => {
                ops = [&expr.ops[..], &self.ops[..]].concat();
                ops.push(DIV);
            }
            _ => { panic!(); }
        }

        Expr { ops: ops, val: val }
    }

    fn to_string(&self, nrs: &[u64]) -> String
    {
        let mut ss = vec![];
        for op in self.ops.iter()
        {
            match *op
            {
                ADD => {
                    let (s0, _) = ss.pop().unwrap();
                    let (s1, _) = ss.pop().unwrap();
                    ss.push((format!("{}+{}", s1, s0), '+'));
                },
                SUB => {
                    let (mut s0, o0) = ss.pop().unwrap();
                    let (s1, _) = ss.pop().unwrap();
                    if "+-".contains(o0)
                    {
                        s0 = format!("({})", s0);
                    }
                    ss.push((format!("{}-{}", s1, s0), '-'));
                },
                MUL => {
                    let (mut s0, o0) = ss.pop().unwrap();
                    let (mut s1, o1) = ss.pop().unwrap();
                    if "+-".contains(o0)
                    {
                        s0 = format!("({})", s0);
                    }
                    if "+-/".contains(o1)
                    {
                        s1 = format!("({})", s1);
                    }
                    ss.push((format!("{}*{}", s1, s0), '*'));
                },
                DIV => {
                    let (mut s0, o0) = ss.pop().unwrap();
                    let (mut s1, o1) = ss.pop().unwrap();
                    if "+-*/".contains(o0)
                    {
                        s0 = format!("({})", s0);
                    }
                    if "+-/".contains(o1)
                    {
                        s1 = format!("({})", s1);
                    }
                    ss.push((format!("{}/{}", s1, s0), '/'));
                }
                idx => {
                    ss.push((nrs[idx as usize].to_string(), 'n'));
                },
            }
        }

        let (res, _) = ss.pop().unwrap();
        res
    }
}

fn partitions(nrs: &[u64], idxs: &[u8]) -> Vec<(Vec<u8>, Vec<u8>)>
{
    let count = nrs.len();
    if count > ::std::u8::MAX as usize - 4
    {
        panic!("Too many numbers for u8 indices");
    }

    let mut res = vec![(vec![nrs[0]], vec![], vec![idxs[0]], vec![])];
    for &idx in idxs[1..].iter()
    {
        let count = res.len();
        res.append(&mut res.clone());
        for (a, _, i, _) in res[..count].iter_mut()
        {
            a.push(nrs[idx as usize]);
            i.push(idx);
        }
        for (a, b, i, j) in res[count..].iter_mut()
        {
            b.push(nrs[idx as usize]);
            j.push(idx);
            if b.len() > a.len() || (b.len() == a.len() && b < a)
            {
                ::std::mem::swap(a, b);
                ::std::mem::swap(i, j);
            }
        }
    }

    res.sort_by_key(|(a, b, _, _)| (a.clone(), b.clone()));
    res.dedup_by_key(|(a, b, _, _)| (a.clone(), b.clone()));

    res.sort_by_key(|(_, b, _, _)| b.len());
    if res[0].1.is_empty()
    {
        res.remove(0);
    }

    res.into_iter().map(|(_, _, i, j)| (i, j)).collect()
}

fn expressions<'a>(nrs: &[u64], idxs: &[u8], cache: &'a mut HashMap<String, Vec<Expr>>) -> String
{
    let key = idxs.iter().map(|&i| nrs[i as usize].to_string()).collect::<Vec<_>>().join("_");
    if !cache.contains_key(&key)
    {
        let mut map = vec![];

        if idxs.len() == 1
        {
            map.push(Expr::new(nrs, idxs[0]));
        }
        else
        {
            let mut seen = ::std::collections::HashSet::with_hasher(Hash64);
            for (idxs0, idxs1) in partitions(nrs, idxs)
            {
                let key0 = expressions(nrs, &idxs0, cache);
                let key1 = expressions(nrs, &idxs1, cache);
                for expr0 in cache[&key0].iter()
                {
                    for expr1 in cache[&key1].iter()
                    {
                        for (op, val) in expr0.possible_combinations(expr1)
                        {
                            if seen.insert(val)
                            {
                                map.push(expr0.combine(expr1, op, val));
                            }
                        }
                    }
                }
            }
        }

// println!("insert {} {}", key, map.len());
        cache.insert(key.clone(), map);
    }

    key
}

fn build_expression(nrs: &[u64], target: u64)
{
    let count = nrs.len();
    if count == 1
    {
        println!("{} = {}", nrs[0], nrs[0]);
    }
    else
    {
        let mut cache = HashMap::new();

        let rtarget = Rat::from_integer(target);
        let mut best = String::new();
        let mut best_diff = Rat::zero();

        let idxs = (0..count as u8).collect::<Vec<_>>();
        'outer: for (idxs0, idxs1) in partitions(nrs, &idxs)
        {

            let key0 = expressions(nrs, &idxs0, &mut cache);
            let key1 = expressions(nrs, &idxs1, &mut cache);
            for expr0 in cache[&key0].iter()
            {
                for expr1 in cache[&key1].iter()
                {
                    for (op, val) in expr0.possible_combinations(expr1)
                    {
                        let diff = if val > rtarget { val - rtarget } else { rtarget - val };
                        if best.is_empty() || diff < best_diff
                        {
                            best = expr0.combine(expr1, op, val).to_string(nrs);
                            best_diff = diff;
                            println!("{} = {}", best, val);

                            if diff.is_zero()
                            {
                                break 'outer;
                            }
                        }
                    }
                }
            }

            cache.remove(&key0);
            if idxs1.len() >= idxs0.len()
            {
                cache.remove(&key1);
            }
        }
    }
}

fn usage() -> !
{
    println!("Usage: makeexpr number [number ...] result");
    ::std::process::exit(1);
}

fn main()
{
    let args: Vec<String> = ::std::env::args().collect();
    if args.len() < 3
    {
        usage()
    }

    let mut nrs = vec![];
    for arg in args[1..args.len()-1].iter()
    {
        match arg.parse::<u64>()
        {
            Ok(nr) => nrs.push(nr),
            _ => usage()
        }
    }

    let target;
    match args.last().unwrap().parse::<u64>()
    {
        Ok(nr) => { target = nr },
        _ => usage()
    }

    nrs.sort();
    build_expression(&nrs, target);
}
