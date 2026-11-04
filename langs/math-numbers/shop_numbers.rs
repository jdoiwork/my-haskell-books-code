// The numbers of the chapter "What numbers really are", in Rust.
// 「数の正体」の章の数を、Rust で

fn main() {
    // ANCHOR: numbers
    // Rust
    let checked = i32::MAX.checked_add(1);   // None
    let wrapped = i32::MAX.wrapping_add(1);  // -2147483648
    let trunc_div = -7 / 2;                  // -3  (like quot)
    let trunc_rem = -7 % 2;                  // -1  (like rem)
    let euclid = (-7i32).div_euclid(2);      // -4  (like div)
    let half = 2.5_f64.round();              // 3.0 (away from 0 / 0 から遠く)
    // ANCHOR_END: numbers
    assert_eq!(checked, None);
    assert_eq!(wrapped, i32::MIN);
    assert_eq!((trunc_div, trunc_rem), (-3, -1));
    assert_eq!((euclid, (-7i32).rem_euclid(2)), (-4, 1));
    assert_eq!(half, 3.0);
    assert_eq!((-2.5_f64).round(), -3.0);
    assert!(0.1 + 0.2 != 0.3_f64);
    println!("ok");
}
