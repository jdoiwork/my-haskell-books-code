// The Rust code in the higher-order chapter.
// 高階関数の章の Rust のコード
fn main() {
let enemies: Vec<i64> = vec![30, 5, 80, 12, 45];
let hps: Vec<i64> = vec![5, 0, 55, 0, 20];
{
        // ANCHOR: functional
        // Rust (functional style)
        // Rust（関数型スタイル）
        let total = hps.iter().fold(0, |acc, hp| acc + hp);
        // ANCHOR_END: functional
assert_eq!(total, 80);
}
{
        // ANCHOR: functional_2
        // Rust (functional style)
        // Rust（関数型スタイル）
        let after_attack: Vec<i64> =
            enemies.iter().map(|hp| (hp - 25).max(0)).collect();
        let alive: Vec<i64> =
            after_attack.into_iter().filter(|&hp| hp > 0).collect();
        let total: i64 = alive.iter().sum();
        // ANCHOR_END: functional_2
assert_eq!(total, 80);
}
{
        // ANCHOR: functional_3
        // Rust (functional style)
        // Rust（関数型スタイル）
        let total: i64 = enemies
            .iter()
            .map(|hp| (hp - 25).max(0))
            .filter(|&hp| hp > 0)
            .sum();
        // ANCHOR_END: functional_3
assert_eq!(total, 80);
}
{
        // ANCHOR: functional_4
        // Rust (functional style)
        // Rust（関数型スタイル）
        let (count, total) = hps.iter().fold((0, 0), |(count, total), &hp| {
            if hp > 0 { (count + 1, total + hp) } else { (count, total) }
        });
        // ANCHOR_END: functional_4
assert_eq!((count, total), (3, 80));
}
}
