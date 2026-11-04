// Remainders in Rust, for the chapter "The world of remainders".
// 「剰余の世界」の章の剰余を、Rust で

// ANCHOR: procedural
// Rust
fn pow_mod(mut base: u64, mut exp: u64, m: u64) -> u64 {
    let mut acc = 1u64;
    base %= m;
    while exp > 0 {
        if exp % 2 == 1 {
            // u64 * u64 can overflow, so multiply in u128
            // u64 どうしの掛け算はあふれることがあるので、u128 で掛ける
            acc = (acc as u128 * base as u128 % m as u128) as u64;
        }
        base = (base as u128 * base as u128 % m as u128) as u64;
        exp /= 2;
    }
    acc
}
// ANCHOR_END: procedural

fn main() {
    assert_eq!(pow_mod(3, 1_000_000, 1_000_000_007), 64935414);
    assert_eq!(pow_mod(65, 17, 3233), 2790);
    assert_eq!(pow_mod(2790, 2753, 3233), 65);
    assert_eq!((-5i32).rem_euclid(24), 19);
    println!("ok");
}
