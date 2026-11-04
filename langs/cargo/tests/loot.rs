// The proptest code in the testing chapter.
// テストの章の proptest のコード
fn split_loot(gold: u32, members: u32) -> Vec<u32> {
    let (share, rest) = (gold / members, gold % members);
    (0..members).map(|i| if i < rest { share + 1 } else { share }).collect()
}
// ANCHOR: property_tests_with_proptest
// Rust (property tests with proptest)
// Rust（proptest で性質のテスト）
use proptest::prelude::*;

proptest! {
    #[test]
    fn keeps_the_total(gold in 0u32..10_000, members in 1u32..100) {
        let total: u32 = split_loot(gold, members).iter().sum();
        prop_assert_eq!(total, gold);
    }
}
// ANCHOR_END: property_tests_with_proptest
