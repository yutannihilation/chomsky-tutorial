use chumsky::prelude::*;

#[derive(Debug)]
enum Expr {
    Num(f64),
    Var(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(String, Vec<Expr>),
    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Box<Expr>,
        then: Box<Expr>,
    },
}

fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident().padded();

    let expr = recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Num(s.parse().unwrap()))
            .padded();

        let atom = int
            .or(expr.delimited_by(just('('), just(')')))
            .or(ident.map(Expr::Var));

        let op = |c| just(c).padded();

        let unary = op('-')
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let mul = op('*').to(Expr::Mul as fn(_, _) -> _);
        let div = op('/').to(Expr::Div as fn(_, _) -> _);
        let add = op('+').to(Expr::Add as fn(_, _) -> _);
        let sub = op('-').to(Expr::Sub as fn(_, _) -> _);

        let product = unary
            .clone()
            .then(mul.or(div).then(unary).repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(add.or(sub).then(product).repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        sum
    })
    .then_ignore(end());

    let decl = recursive(|decl| {
        let r#let = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .then(decl)
            .map(|((name, rhs), then)| Expr::Let {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });
        r#let.or(expr).padded()
    });

    decl.then_ignore(end())
}

fn eval(expr: &Expr) -> Result<f64, String> {
    match expr {
        Expr::Num(x) => Ok(*x),
        Expr::Neg(a) => Ok(-eval(a)?),
        Expr::Add(a, b) => Ok(eval(a)? + eval(b)?),
        Expr::Sub(a, b) => Ok(eval(a)? - eval(b)?),
        Expr::Mul(a, b) => Ok(eval(a)? * eval(b)?),
        Expr::Div(a, b) => Ok(eval(a)? / eval(b)?),
        _ => todo!(), // We'll handle other cases later
    }
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    match parser().parse(src) {
        Ok(ast) => match eval(&ast) {
            Ok(output) => println!("ast:  {ast:?}\neval: {output}"),
            Err(eval_err) => println!("Evaluation error: {}", eval_err),
        },
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {e}")),
    }
}
