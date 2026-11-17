// The Rust code in the chapter on floats.
// 小数の章の Rust のコード

// ANCHOR: procedural
// Rust (procedural style)
// Rust（手続き型スタイル）
fn average(xs: &[i64]) -> f64 {
    let mut total = 0;
    for x in xs {
        total += x;
    }
    // No hidden conversion: say "as f64" yourself.
    // 勝手には変えない。as f64 と自分で書く
    total as f64 / xs.len() as f64
}
// ANCHOR_END: procedural

// ANCHOR: functional
// Rust (functional style)
// Rust（関数型スタイル）
fn average2(xs: &[i64]) -> Option<f64> {
    let total: i64 = xs.iter().sum();
    // checked_div: None for 0, like //? of Lam
    // checked_div は 0 なら None。Lam の //? と同じ
    total.checked_div(xs.len() as i64)?;
    Some(total as f64 / xs.len() as f64)
}
// ANCHOR_END: functional

fn main() {
    let scores = [82, 75, 91, 64];
    assert_eq!(average(&scores), 78.0);
    assert_eq!(average2(&scores), Some(78.0));
    assert_eq!(average2(&[]), None);
    // ANCHOR: surprises
    // Rust (a few surprises)
    // Rust（ちょっとした驚き）
    let whole = -7 / 2;                     // -3: toward 0
    let away = (-2.5f64).round();           // -3.0: away from 0
    let even = 2.5f64.round_ties_even();    // 2.0
    // let mixed = 1 + 2.5;   // error[E0277]: cannot add a float
    //                        //   to an integer
    // ANCHOR_END: surprises
    assert_eq!((whole, away, even), (-3, -3.0, 2.0));
    assert!(f64::NAN.is_nan());
    println!("{} {:?} {} {} {}", average(&scores), average2(&[]), whole, away, even);
}
