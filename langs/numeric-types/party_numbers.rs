// The numbers of the chapter "Number types and conversions", in Rust.
// 「数の型と変換」の章の数を、Rust で

fn main() {
    // ANCHOR: average
    // Rust
    let levels: Vec<i32> = vec![12, 15, 9, 20];
    let total: i32 = levels.iter().sum();
    // as converts by hand, like fromIntegral
    // as は手で変換する（fromIntegral のように）
    let average = total as f64 / levels.len() as f64;  // 14.0
    let damage = (33.0 * 1.5_f64).floor() as i32;      // 49
    // ANCHOR_END: average
    assert_eq!(average, 14.0);
    assert_eq!(damage, 49);

    // ANCHOR: narrow
    // Rust
    // as wraps; try_from refuses, like toIntegralSized
    // as は一周する。try_from は断る（toIntegralSized のように）
    let wrapped = 300_i32 as i8;                // 44
    let checked = i8::try_from(300_i32).ok();   // None
    // From only where it always fits / いつも入りきるときだけ From
    let widened = i64::from(250_i32);           // 250
    // ANCHOR_END: narrow
    assert_eq!(wrapped, 44);
    assert_eq!(checked, None);
    assert_eq!(widened, 250);
    assert_eq!(i8::try_from(99_i32).ok(), Some(99));
    println!("ok");
}
