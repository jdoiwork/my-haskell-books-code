// The Rust code in the Monad intro chapter.
// Monad 入門の章の Rust のコード
fn price(item: &str) -> Option<u32> {
    match item {
        "potion" => Some(50),
        "elixir" => Some(150),
        "sword" => Some(300),
        _ => None,
    }
}

fn pay(cost: u32, gold: u32) -> Option<u32> {
    if cost <= gold { Some(gold - cost) } else { None }
}

// ANCHOR: and_then
// Rust (and_then: Option's (>>=))
// Rust（and_then：Option の (>>=)）
fn buy(item: &str, gold: u32) -> Option<u32> {
    price(item).and_then(|p| pay(p, gold))
}
// ANCHOR_END: and_then

// ANCHOR: question
// Rust (the ? operator: stop here with None, like do)
// Rust（? 演算子：None ならここで止まる。do に近い）
fn buy_three(a: &str, b: &str, c: &str, gold: u32) -> Option<u32> {
    let g1 = buy(a, gold)?;
    let g2 = buy(b, g1)?;
    buy(c, g2)
}
// ANCHOR_END: question

fn main() {
    assert_eq!(buy_three("potion", "elixir", "potion", 300), Some(50));
    assert_eq!(buy_three("potion", "sword", "potion", 300), None);
    assert_eq!(buy_three("potion", "bow", "potion", 300), None);
    println!("ok");
}
