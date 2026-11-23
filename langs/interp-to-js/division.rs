// The Rust code in the chapter "Turning Lam into JavaScript".
// 「JavaScript に変換する」の章の Rust のコード

// ANCHOR: procedural
// Rust (procedural style)
// Rust（手続き型スタイル）
fn factorial(n: u128) -> Option<u128> {
    let mut total: u128 = 1;
    for i in 1..=n {
        // None when it does not fit in 128 bits
        // 128 ビットに入らなければ None
        total = total.checked_mul(i)?;
    }
    Some(total)
}
// ANCHOR_END: procedural

// ANCHOR: functional
// Rust (functional style)
// Rust（関数型スタイル）
fn factorial2(n: u128) -> u128 {
    (1..=n).product()
}

fn halves() -> (i64, i64) {
    // / goes toward 0; div_euclid keeps the rest 0 or more.
    // / は 0 の方へ。div_euclid は余りを 0 以上にする
    (-7 / 2, (-7i64).div_euclid(2))
}
// ANCHOR_END: functional

fn main() {
    assert_eq!(factorial(25), Some(15511210043330985984000000));
    assert_eq!(factorial2(25), 15511210043330985984000000);
    assert_eq!(factorial(40), None);
    assert_eq!(halves(), (-3, -4));
    assert_eq!((7i64).div_euclid(-2), -3);
    println!("{:?} {:?}", factorial(25), halves());
}
