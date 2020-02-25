use num_traits::{Zero, One};
use std::collections::HashMap;
use arrayvec::ArrayVec;
use fasthash::xx::Hash64;

type Rat = num_rational::Ratio<u64>;
/// Type for an index.
///
/// Values of this type are used as an index in the array of input numbers.
/// The highest four possible index values are reserved for encoding the
/// operations.In the (highly unlikely) case that you wish to use this program
/// with more than 252 input numbers, change this type to `u16` or wider.
/// Typically, though, memory or time constraints limit the use of this program
/// to approximately 10 distinct input numbers.
type Idx = u8;
/// The type for a single operation.
///
/// Values of this type are used either as an index in the array of input
/// numbers, or are one of the special values `ADD`..`DIV` that indicate
/// an operation on the previous two values in the stack.
type Op = Idx;

const ADD: Op = Op::max_value();
const SUB: Op = Op::max_value() - 1;
const MUL: Op = Op::max_value() - 2;
const DIV: Op = Op::max_value() - 3;


/// Structure describing an expression
///
/// Struct `Expr` stores an expression and the value it evaluates to. The
/// expression is stored in reverse polish notation, and uses indices into
/// a numbers array instead of the actual numbers themselves. The operators
/// in the expression are encoded as the 4 greatest numbers that can be encoded
/// in the index type. Thus, an expression like
/// ```
/// [0, 3, ADD, 2, MUL]
/// ```
/// will for a numbers array `nrs` evaluate to
/// ```
/// (nrs[0] + nrs[3]) * nrs[2]
/// ```
#[derive(Clone)]
struct Expr
{
    /// The expression itself
    ops: Vec<Op>,
    /// The resulting value of the expression
    val: Rat
}

impl Expr
{
    /// Create a new expression.
    ///
    /// Create a new expression for the single number `nrs[idx]`.
    fn new(nrs: &[u64], idx: Idx) -> Self
    {
        Expr { ops: vec![idx], val: Rat::from_integer(nrs[idx as usize]) }
    }

    /// Create an empty expression.
    ///
    /// Create an empty expression that evaluates to zero.
    fn empty() -> Self
    {
        Expr { ops: vec![], val: Rat::zero() }
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

fn partitions(idxs: &[Idx]) -> Vec<(Vec<Idx>, Vec<Idx>)>
{
    let mut res = vec![(vec![idxs[0]], vec![])];
    for &idx in idxs[1..].iter()
    {
        let count = res.len();
        res.append(&mut res.clone());
        for (a, _) in res[..count].iter_mut()
        {
            a.push(idx);
        }
        for (a, b) in res[count..].iter_mut()
        {
            b.push(idx);
            if b.len() > a.len() || (b.len() == a.len() && b < a)
            {
                ::std::mem::swap(a, b);
            }
        }
    }

    res.sort();
    res.dedup();

    res.sort_by_key(|(_, b)| b.len());
    if res[0].1.is_empty()
    {
        res.remove(0);
    }

    res
}

fn expressions<'a>(nrs: &[u64], idxs: &[Idx],
    cache: &'a mut HashMap<String, Vec<Expr>>) -> String
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
            for (idxs0, idxs1) in partitions(idxs)
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

/// Find the indexes of the unique numbers in an array.
///
/// For all elements in array `nrs`, find the index of the first occurrence of
/// that element in `nrs`, and store it in the result. Afterwards, the indices
/// array is sorted. Thus, equal numbers in the input array result in indices
/// occuring with the same frequency in the output array, though not necessarily
/// in the same order.
fn unique_indices(nrs: &[u64]) -> Vec<Idx>
{
    let count = nrs.len();
    let mut res = vec![];
    for idx in 0..count
    {
        let uniq_idx = match nrs.iter().position(|&x| x == nrs[idx])
            {
                Some(dup_idx) => dup_idx,
                None          => idx
            };
        res.push(uniq_idx as Idx);
    }

    res.sort();
    res
}

fn get_nearest_expression_multiple(nrs: &[u64], target: u64) -> Expr
{
    let mut cache = HashMap::new();

    let rtarget = Rat::from_integer(target);
    let mut best = Expr::empty();
    let mut best_diff = Rat::from_integer(::std::u64::MAX);

    let idxs = unique_indices(nrs);
    'outer: for (idxs0, idxs1) in partitions(&idxs)
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
                    if diff < best_diff
                    {
                        best = expr0.combine(expr1, op, val);
                        best_diff = diff;
                        println!("{} = {}", best.to_string(nrs), val);

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

    best
}

fn get_nearest_expression_2(nrs: &[u64], target: u64) -> Expr
{
    let rtarget = Rat::from_integer(target);
    let mut best = Expr::empty();
    let mut best_diff = Rat::from_integer(::std::u64::MAX);

    let expr0 = Expr::new(nrs, 0);
    let expr1 = Expr::new(nrs, 1);
    for (op, val) in expr0.possible_combinations(&expr1)
    {
        let diff = if val > rtarget { val - rtarget } else { rtarget - val };
        if diff < best_diff
        {
            best = expr0.combine(&expr1, op, val);
            best_diff = diff;
            if diff.is_zero()
            {
                break;
            }
        }
    }

    best
}

fn get_nearest_expression(nrs: &[u64], target: u64) -> Expr
{
    match nrs.len()
    {
        1 => Expr::new(nrs, 0),
        2 => get_nearest_expression_2(nrs, target),
        _ => get_nearest_expression_multiple(nrs, target)
    }
}

/// Print a usage message, and exit.
fn usage() -> !
{
    println!("Usage: makeexpr number [number ...] target");
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

    let count = nrs.len();
    if count > Op::max_value() as usize - 4
    {
        panic!("Too many numbers for {}-bit indices", 8*::std::mem::size_of::<Idx>());
    }

    let target;
    match args.last().unwrap().parse::<u64>()
    {
        Ok(nr) => { target = nr },
        _ => usage()
    }

    let expr = get_nearest_expression(&nrs, target);
    println!("{} = {}", expr.to_string(&nrs), expr.val);
}
