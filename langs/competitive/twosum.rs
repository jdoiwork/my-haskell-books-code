// The Rust code in the competitive programming chapter.
// 競技プログラミングの章の Rust のコード
use std::collections::HashMap;

// ANCHOR: twosum
// Rust (LeetCode 1, Two Sum, with a HashMap)
// Rust（LeetCode の1問目 Two Sum を、HashMap で）
fn two_sum(nums: &[i32], target: i32) -> Option<(usize, usize)> {
    let mut seen: HashMap<i32, usize> = HashMap::new();
    for (i, &x) in nums.iter().enumerate() {
        if let Some(&j) = seen.get(&(target - x)) {
            return Some((j, i));
        }
        seen.insert(x, i);
    }
    None
}
// ANCHOR_END: twosum

fn main() {
    assert_eq!(two_sum(&[2, 7, 11, 15], 9), Some((0, 1)));
    assert_eq!(two_sum(&[3, 2, 4], 6), Some((1, 2)));
    assert_eq!(two_sum(&[1, 2], 10), None);
    println!("ok");
}
