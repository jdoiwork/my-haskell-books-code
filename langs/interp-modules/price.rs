// ANCHOR: all
// Rust: a module, with pub for what others may use
// Rust：モジュール。ほかが使ってよいものに pub を付ける
mod price {
    use std::fmt;

    // The struct is pub, but its field yen is not: outside, nobody
    // can write Price { yen: -5 }.
    // 構造体は pub だが、フィールド yen は pub でない。外では
    // Price { yen: -5 } と書けない
    pub struct Price {
        yen: i64,
    }

    pub fn from_yen(n: i64) -> Option<Price> {
        if n < 0 { None } else { Some(Price { yen: n }) }
    }

    pub fn add(a: &Price, b: &Price) -> Price {
        Price { yen: a.yen + b.yen }
    }

    pub fn yen(p: &Price) -> i64 {
        p.yen
    }

    // Lam's  instance Display Price: format!("{p}") calls it. It goes
    // wherever Price goes, with no use line of its own.
    // Lam の instance Display Price と同じ。format!("{p}") が呼ぶ。
    // 自分の use の行がなくても、Price の行くところへ付いていく
    impl fmt::Display for Price {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{} yen", self.yen)
        }
    }
}

// The two lines together are Lam's  import Price as P (fromYen).
// 2行で、Lam の  import Price as P (fromYen)  と同じ
use price as p;          // p::from_yen, p::add
use price::from_yen;     // from_yen

fn main() {
    let prices = [from_yen(1200), from_yen(350), p::from_yen(-5)];
    let lines: Vec<String> = prices
        .iter()
        .map(|m| match m {
            Some(x) => format!("{x}"),
            None => "not a price".to_string(),
        })
        .collect();
    for line in &lines {
        println!("{line}");
    }
    let both = p::add(&p::from_yen(1).unwrap(), &p::from_yen(2).unwrap());
    assert_eq!(format!("{both}"), "3 yen");
    assert_eq!(p::yen(&both), 3);
    assert_eq!(lines, ["1200 yen", "350 yen", "not a price"]);
    // let cheat = p::Price { yen: -5 };   // error[E0451]: private field
}
// ANCHOR_END: all
