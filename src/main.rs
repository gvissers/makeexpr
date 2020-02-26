//! A program for creating arithemtic expression that evaluate to a certain number,
//!
//! This program receives a list of input numbers and a target number. Using
//! all input numbers it creates an arithmetic expression using only addition,
//! subtraction, multiplication and division operations, that evaluates to the
//! target number. In case this is not possible, it finds the expression that
//! evaluates to a number that is a close to the target as possible.
//!
//! The program can be used by passing the input numbers and the target number
//! on the command line:
//! ```
//! makeexpr number [number ...] target
//! ```
//! The best expression found will then be printed on stdout.
//! As an example,
//! ```
//! makeexpr 1 3 4 6 24
//! ```
//! will result in output
//! ```
//! 6/(1-3/4) = 24
//! ```


use std::collections::HashMap;
use arrayvec::ArrayVec;
use fasthash::xx::Hash64;
use num_traits::Zero;

/// Type alias for a rational number (i.e. fraction)
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

/// Wrapper for hashing rational numbers.
///
/// The default hash function for Ratio<T> goes out of its way to ensure that
/// unnormalized numbers give the same hash as their normalized counterparts.
/// However, the numbers constructed in this program are all normalized,
/// so we can get away with a much simpler hashing function. To immplement that,
/// the number is wrapped in a wrapper type, and a simple hash implementation is
/// provided for the wrapper.
struct NormalizedRat(Rat);

impl PartialEq for NormalizedRat
{
    fn eq(&self, other: &Self) -> bool
    {
        self.0 == other.0
    }
}
impl Eq for NormalizedRat {}
impl ::std::hash::Hash for NormalizedRat
{
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H)
    {
        state.write_u64(*self.0.numer());
        state.write_u64(*self.0.denom());
    }
}

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
        let ops = match (op0, op1)
            {
                (ADD, ADD) => "*/\\",
                (ADD, SUB) => "*/\\",
                (ADD, MUL) => "+-\\",
                (ADD, DIV) => "+-\\",
                (ADD,   _) => "+-*/\\",
                (SUB, ADD) => "*/\\",
                (SUB, SUB) => "*/\\",
                (SUB, MUL) => "-\\",
                (SUB, DIV) => "-\\",
                (SUB,   _) => "-*/\\",
                (MUL, ADD) => "*/",
                (MUL, SUB) => "*/",
                (MUL, MUL) => "+-_",
                (MUL, DIV) => "+-_",
                (MUL,   _) => "+-*/_",
                (DIV, ADD) => "/_",
                (DIV, SUB) => "/_",
                (DIV, MUL) => "+-_",
                (DIV, DIV) => "+-_",
                (DIV,   _) => "+-/_",
                _          => "+-*/_\\"
            };

        for op in ops.chars()
        {
            match op
            {
                '+' => {
                    res.push((op, self.val + expr.val));
                },
                '-' => {
                    if self.val >= expr.val
                    {
                        res.push((op, self.val - expr.val));
                    }
                },
                '*' => {
                    res.push((op, self.val * expr.val));
                },
                '/' => {
                    if !expr.val.is_zero()
                    {
                        res.push((op, self.val / expr.val));
                    }
                },
                '_' => {
                    if expr.val >= self.val
                    {
                        res.push((op, expr.val - self.val));
                    }
                },
                '\\' => {
                    if !self.val.is_zero()
                    {
                        res.push((op, expr.val / self.val));
                    }
                },
                _ => {}
            }
        }

        res
    }

    fn combine(&self, expr: &Self, op: char, val: Rat) -> Self
    {
        let ops = match op
            {
                '+' => {
                    [&self.ops[..], &expr.ops[..], &[ADD]].concat()
                },
                '-' => {
                    [&self.ops[..], &expr.ops[..], &[SUB]].concat()
                },
                '*' => {
                    [&self.ops[..], &expr.ops[..], &[MUL]].concat()
                },
                '/' => {
                    [&self.ops[..], &expr.ops[..], &[DIV]].concat()
                },
                '_' => {
                    [&expr.ops[..], &self.ops[..], &[SUB]].concat()
                },
                '\\' => {
                    [&expr.ops[..], &self.ops[..], &[DIV]].concat()
                }
                _ => { panic!(); }
            };

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
                            if seen.insert(NormalizedRat(val))
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

/// Find the expression nearest to target.
///
/// Given more than two input numbers in `nrs`, and target number `target`,
/// find an arithmetic expression using all the numbers in `nrs` that evaluates
/// to a number as close as possible (or equal to) `target`. If
/// `print_intermediate` is true, intermediate search  results are printed on
/// `stdout`.
fn get_nearest_expression_multiple(nrs: &[u64], target: u64, print_intermediate: bool) -> Expr
{
    let mut cache = HashMap::new();

    let rtarget = Rat::from_integer(target);
    let mut best = Expr::empty();
    let mut best_min = Rat::zero();
    let mut best_max = Rat::from_integer(::std::u64::MAX);

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
                    if val > best_min && val < best_max
                    {
                        let diff = if val < rtarget { rtarget - val } else { val - rtarget };

                        best = expr0.combine(expr1, op, val);
                        best_min = if diff > rtarget { Rat::zero() } else { rtarget - diff };
                        best_max = rtarget + diff;

                        if diff.is_zero()
                        {
                            break 'outer;
                        }

                        if print_intermediate
                        {
                            println!("{} = {}", best.to_string(nrs), val);
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

/// Find the expression nearest to target.
///
/// Given two input numbers in `nrs`, and target number `target`, find an arithmetic
/// expression using all the numbers in `nrs` that evaluates to a number as close
/// as possible (or equal to) `target`. If `print_intermediate` is true,
/// intermediate search  results are printed on `stdout`.
fn get_nearest_expression_2(nrs: &[u64], target: u64, print_intermediate: bool) -> Expr
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

            if print_intermediate
            {
                println!("{} = {}", best.to_string(nrs), val);
            }
        }
    }

    best
}

/// Find the expression nearest to target.
///
/// Given input numbers `nrs`, and target number `target`, find an arithmetic
/// expression using all the numbers in `nrs` that evaluates to a number as close
/// as possible (or equal to) `target`. If `print_intermediate` is true,
/// intermediate search  results are printed on `stdout`.
fn get_nearest_expression(nrs: &[u64], target: u64, print_intermediate: bool) -> Expr
{
    match nrs.len()
    {
        1 => Expr::new(nrs, 0),
        2 => get_nearest_expression_2(nrs, target, print_intermediate),
        _ => get_nearest_expression_multiple(nrs, target, print_intermediate)
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
    let print_intermediate = true;
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

    let expr = get_nearest_expression(&nrs, target, print_intermediate);
    println!("{} = {}", expr.to_string(&nrs), expr.val);
}
