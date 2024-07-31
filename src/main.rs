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

        let call = ident
            .then(
                expr.clone()
                    .separated_by(just(','))
                    .allow_trailing()
                    .delimited_by(just('('), just(')')),
            )
            .map(|(f, args)| Expr::Call(f, args));

        let atom = int
            .or(expr.delimited_by(just('('), just(')')))
            .or(call)
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
    });

    let decl = recursive(|decl| {
        let r#let = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .then(decl.clone())
            .map(|((name, rhs), then)| Expr::Let {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });

        let r#fn = text::keyword("fn")
            .ignore_then(ident)
            .then(ident.repeated())
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .then(decl)
            .map(|(((name, args), body), then)| Expr::Fn {
                name,
                args,
                body: Box::new(body),
                then: Box::new(then),
            });

        r#let.or(r#fn).or(expr).padded()
    });

    decl.then_ignore(end())
}

fn eval<'a>(
    expr: &'a Expr,
    vars: &mut Vec<(&'a String, f64)>,
    fns: &mut Vec<(&'a String, &'a [String], &'a Expr)>,
) -> Result<f64, String> {
    match expr {
        Expr::Num(x) => Ok(*x),
        Expr::Neg(a) => Ok(-eval(a, vars, fns)?),
        Expr::Add(a, b) => Ok(eval(a, vars, fns)? + eval(b, vars, fns)?),
        Expr::Sub(a, b) => Ok(eval(a, vars, fns)? - eval(b, vars, fns)?),
        Expr::Mul(a, b) => Ok(eval(a, vars, fns)? * eval(b, vars, fns)?),
        Expr::Div(a, b) => Ok(eval(a, vars, fns)? / eval(b, vars, fns)?),

        Expr::Var(name) => {
            if let Some((_, val)) = vars.iter().rev().find(|(var, _)| *var == name) {
                Ok(*val)
            } else {
                Err(format!("Cannot find variable `{name}` in scope"))
            }
        }

        Expr::Let { name, rhs, then } => {
            let rhs = eval(rhs, vars, fns)?;
            vars.push((name, rhs));
            let output = eval(then, vars, fns);
            vars.pop();
            output
        }

        Expr::Call(name, args) => {
            let fn_ = fns.iter().rev().find(|(var, _, _)| *var == name);

            if fn_.is_none() {
                return Err(format!("Cannot find function `{}` in scope", name));
            }

            let (_, arg_names, body) = fn_.copied().unwrap();

            if arg_names.len() != args.len() {
                return Err(format!(
                    "Wrong number of arguments for function `{name}`: expected {}, found {}",
                    arg_names.len(),
                    args.len(),
                ));
            }

            let mut args_evaled = args
                .iter()
                .map(|arg| eval(arg, vars, fns))
                .zip(arg_names.iter())
                .map(|(var, name)| Ok((name, var?)))
                .collect::<Result<_, String>>()?;

            vars.append(&mut args_evaled);
            let output = eval(&body, vars, fns);
            vars.truncate(vars.len() - args_evaled.len());
            output
        }

        Expr::Fn {
            name,
            args,
            body,
            then,
        } => {
            fns.push((name, args, body));
            let output = eval(then, vars, fns);
            fns.pop();
            output
        }

        _ => todo!(), // We'll handle other cases later
    }
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let mut vars = Vec::new();
    let mut fns = Vec::new();

    match parser().parse(src) {
        Ok(ast) => match eval(&ast, &mut vars, &mut fns) {
            Ok(output) => println!("ast:  {ast:?}\neval: {output}"),
            Err(eval_err) => println!("Evaluation error: {}", eval_err),
        },
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {e}")),
    }
}
