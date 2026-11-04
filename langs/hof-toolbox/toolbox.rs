// The Rust code in the hof-toolbox chapter.
// 高階関数の道具箱の章の Rust のコード
fn damage(attack: i64, defense: i64) -> i64 {
    (attack - defense).max(1)
}
fn main() {
let attacks: Vec<i64> = vec![50, 35, 60];
let defenses: Vec<i64> = vec![20, 40, 10];
let hp_log: Vec<i64> = vec![100, 70, 40, 10, 0, 0];
let battle_log = [(1, "Goblin-A"), (1, "Goblin-B"), (2, "Goblin-C"), (3, "Goblin-D"), (3, "Goblin-E")];
{
        // ANCHOR: functional
        // Rust (functional style)
        // Rust（関数型スタイル）
        let results: Vec<i64> = attacks
            .iter()
            .zip(defenses.iter())
            .map(|(&a, &d)| damage(a, d))
            .collect();
        // ANCHOR_END: functional
assert_eq!(results, [30, 1, 50]);
}
{
        // ANCHOR: functional_2
        // Rust (functional style)
        // Rust（関数型スタイル）
        let cut = hp_log.iter().position(|&hp| hp <= 0).unwrap_or(hp_log.len());
        let (alive, after) = hp_log.split_at(cut);
        // ANCHOR_END: functional_2
assert_eq!(alive, [100, 70, 40, 10]);
assert_eq!(after, [0, 0]);
}
{
        // ANCHOR: functional_3
        // Rust (functional style)
        // Rust（関数型スタイル）
        let history: Vec<i64> = std::iter::successors(Some(100), |&hp| {
            if hp - 30 > 0 { Some(hp - 30) } else { None }
        })
        .collect();
        // ANCHOR_END: functional_3
assert_eq!(history, [100, 70, 40, 10]);
}
{
        // ANCHOR: functional_4
        // Rust (functional style)
        // Rust（関数型スタイル）
        let groups: Vec<_> = battle_log.chunk_by(|a, b| a.0 == b.0).collect();
        // ANCHOR_END: functional_4
assert_eq!(groups.iter().map(|g| g.len()).collect::<Vec<_>>(), [2, 1, 2]);
}
}
