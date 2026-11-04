// The Rust code in the language design chapter.
// 言語を設計する章の Rust のコード
use std::collections::BTreeSet;

// ANCHOR: enum
// Rust (an enum; a child Expr goes in a Box)
// Rust（enum。子の Expr は Box に入れる）
enum Expr {
    Int(i64),
    Var(String),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
}

fn free_vars(e: &Expr) -> BTreeSet<String> {
    match e {
        Expr::Int(_) => BTreeSet::new(),
        Expr::Var(x) => BTreeSet::from([x.clone()]),
        Expr::Lam(x, body) => {
            let mut s = free_vars(body);
            s.remove(x);
            s
        }
        Expr::App(f, a) | Expr::Add(f, a) => {
            let mut s = free_vars(f);
            s.extend(free_vars(a));
            s
        }
    }
}
// ANCHOR_END: enum

fn sum_ints(e: &Expr) -> i64 {
    match e {
        Expr::Int(n) => *n,
        Expr::Var(_) => 0,
        Expr::Lam(_, b) => sum_ints(b),
        Expr::App(f, a) | Expr::Add(f, a) => sum_ints(f) + sum_ints(a),
    }
}

fn main() {
    let e = Expr::Lam(
        "x".to_string(),
        Box::new(Expr::Add(
            Box::new(Expr::Var("x".to_string())),
            Box::new(Expr::Var("y".to_string())),
        )),
    );
    let app = Expr::App(Box::new(Expr::Int(1)), Box::new(Expr::Int(2)));
    assert_eq!(sum_ints(&app), 3);
    assert_eq!(free_vars(&e), BTreeSet::from(["y".to_string()]));
    println!("ok");
}
