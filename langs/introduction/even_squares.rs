// The comparison code in the introduction.
// 「はじめに」の比較のコード
fn main() {
{
        // ANCHOR: procedural
        // Rust (procedural style)
        // Rust（手続き型スタイル）
        let mut even_squares = Vec::new();
        for x in 1..=10 {
            if x % 2 == 0 {
                even_squares.push(x * x);
            }
        }
        // ANCHOR_END: procedural
assert_eq!(even_squares, vec![4, 16, 36, 64, 100]);
}
{
        // ANCHOR: functional
        // Rust (functional style)
        // Rust（関数型スタイル）
        let even_squares: Vec<i64> = (1..=10)
            .filter(|x| x % 2 == 0)
            .map(|x| x * x)
            .collect();
        // ANCHOR_END: functional
assert_eq!(even_squares, vec![4, 16, 36, 64, 100]);
}
}
