use num_traits::Zero;

type Rat = num_rational::Ratio<u64>;

#[derive(Debug)]
enum Op
{
    Push(usize),
    Add,
    Sub,
    Mul,
    Div,
    RDiv
}

struct State
{
    nrs: Vec<u64>,
    target: u64,
    in_use: Vec<bool>,
    expr: Vec<Op>,
    stack: Vec<Rat>,
    rstack: Vec<Rat>,
    best: String,
    best_diff: Option<Rat>
}

impl State
{
    fn new(mut nrs: Vec<u64>, target: u64) -> Self
    {
        nrs.sort();
        nrs.reverse();

        let count = nrs.len();
        State
        {
            nrs: nrs,
            target: target,
            in_use: vec![false; count],
            expr: vec![],
            stack: vec![],
            rstack: vec![],
            best: String::new(),
            best_diff: None
        }
    }

    fn push(&mut self, idx: usize)
    {
        self.expr.push(Op::Push(idx));
        self.in_use[idx] = true;
        self.stack.push(Rat::from_integer(self.nrs[idx]));
//         println!("Push:");
//         println!("expr = {:?}", self.expr);
//         println!("stack = {:?}", self.stack);
    }

    fn add(&mut self)
    {
        self.expr.push(Op::Add);
        let n0 = self.stack.pop().unwrap();
        let n1 = self.stack.pop().unwrap();
        self.stack.push(n1 + n0);
        self.rstack.push(n1);
        self.rstack.push(n0);
//         println!("Add:");
//         println!("expr = {:?}", self.expr);
//         println!("stack = {:?}", self.stack);
    }

    fn sub(&mut self)
    {
        self.expr.push(Op::Sub);
        let n0 = self.stack.pop().unwrap();
        let n1 = self.stack.pop().unwrap();
        self.stack.push(n1 - n0);
        self.rstack.push(n1);
        self.rstack.push(n0);
//         println!("Sub:");
//         println!("expr = {:?}", self.expr);
//         println!("stack = {:?}", self.stack);
    }

    fn mul(&mut self)
    {
        self.expr.push(Op::Mul);
        let n0 = self.stack.pop().unwrap();
        let n1 = self.stack.pop().unwrap();
        self.stack.push(n1 * n0);
        self.rstack.push(n1);
        self.rstack.push(n0);
//         println!("Mul:");
//         println!("expr = {:?}", self.expr);
//         println!("stack = {:?}", self.stack);
    }

    fn div(&mut self)
    {
        self.expr.push(Op::Div);
        let n0 = self.stack.pop().unwrap();
        let n1 = self.stack.pop().unwrap();
        self.stack.push(n1 / n0);
        self.rstack.push(n1);
        self.rstack.push(n0);
//         println!("Div:");
//         println!("expr = {:?}", self.expr);
//         println!("stack = {:?}", self.stack);
    }

    fn rdiv(&mut self)
    {
        self.expr.push(Op::RDiv);
        let n0 = self.stack.pop().unwrap();
        let n1 = self.stack.pop().unwrap();
        self.stack.push(n0 / n1);
        self.rstack.push(n1);
        self.rstack.push(n0);
//         println!("RDiv:");
//         println!("expr = {:?}", self.expr);
//         println!("stack = {:?}", self.stack);
    }

    fn undo(&mut self)
    {
        let op = self.expr.pop().unwrap();
        match op
        {
            Op::Push(idx) => {
                self.stack.pop();
                self.in_use[idx] = false;
            },
            _ => {
                self.stack.pop();
                let n0 = self.rstack.pop().unwrap();
                let n1 = self.rstack.pop().unwrap();
                self.stack.push(n1);
                self.stack.push(n0);
            }
        }
//         println!("Undo:");
//         println!("expr = {:?}", self.expr);
//         println!("stack = {:?}", self.stack);
    }

    fn to_string(&self) -> String
    {
        let mut ss = vec![];
        for op in self.expr.iter()
        {
            match op
            {
                Op::Push(idx) => {
                    ss.push((self.nrs[*idx].to_string(), 'n'));
                },
                Op::Add => {
                    let (s0, _) = ss.pop().unwrap();
                    let (s1, _) = ss.pop().unwrap();
                    ss.push((format!("{}+{}", s1, s0), '+'));
                },
                Op::Sub => {
                    let (mut s0, o0) = ss.pop().unwrap();
                    let (s1, _) = ss.pop().unwrap();
                    if "+-".contains(o0)
                    {
                        s0 = format!("({})", s0);
                    }
                    ss.push((format!("{}-{}", s1, s0), '-'));
                },
                Op::Mul => {
                    let (mut s0, o0) = ss.pop().unwrap();
                    let (mut s1, o1) = ss.pop().unwrap();
                    if "+-".contains(o0)
                    {
                        s0 = format!("({})", s0);
                    }
                    if "+-*/\\".contains(o1)
                    {
                        s1 = format!("({})", s1);
                    }
                    ss.push((format!("{}*{}", s1, s0), 'm'));
                },
                Op::Div => {
                    let (mut s0, o0) = ss.pop().unwrap();
                    let (mut s1, o1) = ss.pop().unwrap();
                    if "+-*/\\".contains(o0)
                    {
                        s0 = format!("({})", s0);
                    }
                    if "+-*/\\".contains(o1)
                    {
                        s1 = format!("({})", s1);
                    }
                    ss.push((format!("{}/{}", s1, s0), '/'));
                }
                Op::RDiv => {
                    let (mut s0, o0) = ss.pop().unwrap();
                    let (mut s1, o1) = ss.pop().unwrap();
                    if "+-*/\\".contains(o0)
                    {
                        s0 = format!("({})", s0);
                    }
                    if "+-*/\\".contains(o1)
                    {
                        s1 = format!("({})", s1);
                    }
                    ss.push((format!("{}\\{}", s1, s0), '\\'));
                }
            }
        }

        let (res, _) = ss.pop().unwrap();
        res
    }

    fn try_build_all_from(&mut self, todo: usize) -> bool
    {
        let depth = self.stack.len();

        if todo == 0 && depth == 1
        {
            let diff = if self.stack[0] > Rat::from_integer(self.target)
                { self.stack[0] - self.target }
                else { Rat::from_integer(self.target) - self.stack[0] };
            if self.best_diff.is_none() || diff < self.best_diff.unwrap()
            {
                self.best = self.to_string();
                self.best_diff = Some(diff);
                println!("{} = {}", self.best, self.stack[0]);
            }
            diff.is_zero()
        }
        else
        {
            if depth >= 2 && self.stack[depth-2] >= self.stack[depth-1]
            {
                self.add();
                if self.try_build_all_from(todo)
                {
                    return true;
                }
                self.undo();

                self.sub();
                if self.try_build_all_from(todo)
                {
                    return true;
                }
                self.undo();

                self.mul();
                if self.try_build_all_from(todo)
                {
                    return true;
                }
                self.undo();

                if !self.stack[depth-1].is_zero()
                {
                    self.div();
                    if self.try_build_all_from(todo)
                    {
                        return true;
                    }
                    self.undo();
                }

                if !self.stack[depth-2].is_zero()
                {
                    self.rdiv();
                    if self.try_build_all_from(todo)
                    {
                        return true;
                    }
                    self.undo();
                }
            }

            if todo > 0
            {
                let count = self.nrs.len();
                let mut last = self.nrs[0] + 1;
                for idx in 0..count
                {
                    if !self.in_use[idx] && self.nrs[idx] < last
                    {
                        self.push(idx);
                        if self.try_build_all_from(todo-1)
                        {
                            return true;
                        }
                        self.undo();
                        last = self.nrs[idx];
                    }
                }
            }

            false
        }
    }

    fn try_build_all(&mut self) -> &str
    {
        self.best = String::new();

        let count = self.nrs.len();
        let mut last = self.nrs[0] + 1;
        for idx in 0..count
        {
            if self.nrs[idx] < last
            {
                self.push(idx);
                if self.try_build_all_from(count - 1)
                {
                    break;
                }
                self.undo();
                last = self.nrs[idx];
            }
        }

        &self.best
    }
}

fn build_expression(nrs: Vec<u64>, target: u64)
{
    let mut state = State::new(nrs, target);
    state.try_build_all();
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

    build_expression(nrs, target);
}
