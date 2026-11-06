// The fast inverse square root of the chapter "Numbers", in Rust.
// 「数の正体」の章の高速逆平方根を、Rust で

// ANCHOR: functional
// Rust
fn fast_inv_sqrt(x: f32) -> f32 {
    let bits = 0x5f3759df - (x.to_bits() >> 1);
    let guess = f32::from_bits(bits);
    guess * (1.5 - 0.5 * x * guess * guess)
}
// ANCHOR_END: functional

fn main() {
    for x in [0.25_f32, 2.0, 100.0, 12345.0] {
        assert!((fast_inv_sqrt(x) * x.sqrt() - 1.0).abs() < 0.002);
    }
    println!("ok");
}
