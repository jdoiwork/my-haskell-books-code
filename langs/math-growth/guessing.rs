// log2 and binary search in Rust, for the chapter
// "Exponents and logarithms".
// 「指数と対数」の章の log2 と二分探索を、Rust で

fn main() {
    // ANCHOR: functional
    // Rust
    let halvings = 1000u32.ilog2();               // 9
    let levels = [100, 200, 400, 800, 1600, 3200];
    let found = levels.binary_search(&800);       // Ok(3)
    let missing = levels.binary_search(&1000);    // Err(4): would go at 4
    // ANCHOR_END: functional
    assert_eq!(halvings, 9);
    assert_eq!(found, Ok(3));
    assert_eq!(missing, Err(4));
    println!("ok");
}
