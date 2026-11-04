// The Rust code in the measuring and tuning chapter.
// 計測とチューニングの章の Rust のコード
use std::collections::BTreeSet;
use std::time::Instant;

fn main() {
    let hits: Vec<(String, u32)> = (1..=20000u32)
        .map(|i| (format!("hero-{}", i % 2000), (i * 37) % 100))
        .collect();

    // ANCHOR: instant
    // Rust (Instant: a clock for measuring)
    // Rust（Instant：測るための時計）
    let start = Instant::now();
    let heroes: BTreeSet<&str> =
        hits.iter().map(|(name, _)| name.as_str()).collect();
    let took = start.elapsed();
    // ANCHOR_END: instant
    assert_eq!(heroes.len(), 2000);
    assert!(took.as_secs() < 1);
    println!("ok");
}
