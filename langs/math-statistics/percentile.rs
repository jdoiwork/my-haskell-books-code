// The p99 of the chapter "Basic statistics", in Rust.
// 「統計の基本」の章の p99 を、Rust で

// ANCHOR: percentile
// Rust
fn percentile(p: f64, xs: &[f64]) -> f64 {
    let mut sorted = xs.to_vec();
    // f64 is not Ord (NaN), so sort with total_cmp.
    // f64 は Ord ではない（NaN がある）ので、total_cmp で並べる
    sorted.sort_by(|a, b| a.total_cmp(b));
    let rank = (p / 100.0 * sorted.len() as f64).ceil() as usize;
    sorted[rank.max(1) - 1]
}
// ANCHOR_END: percentile

fn main() {
    let times: Vec<f64> = (1..=100).map(|t| t as f64).collect();
    assert_eq!(percentile(90.0, &times), 90.0);
    assert_eq!(percentile(99.0, &times), 99.0);
    assert_eq!(percentile(1.0, &times), 1.0);
    println!("ok");
}
