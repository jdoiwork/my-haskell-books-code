// Fast power with its loop invariant checked by debug_assert!, in Rust.
// 速いべき乗を、ループ不変条件を debug_assert! で確かめながら、Rust で

// ANCHOR: procedural
// Rust
fn power(x: i128, n: u32) -> i128 {
    let (mut acc, mut b, mut e) = (1i128, x, n);
    while e > 0 {
        // the invariant (checked only in debug builds)
        // 不変条件（デバッグビルドのときだけ確かめる）
        debug_assert_eq!(acc * b.pow(e), x.pow(n));
        if e % 2 == 0 {
            b *= b;
            e /= 2;
        } else {
            acc *= b;
            e -= 1;
        }
    }
    acc
}
// ANCHOR_END: procedural

fn main() {
    assert_eq!(power(2, 10), 1024);
    assert_eq!(power(3, 5), 243);
    for x in -5..=5 {
        for n in 0..=20 {
            assert_eq!(power(x, n), x.pow(n));
        }
    }
    println!("ok");
}
