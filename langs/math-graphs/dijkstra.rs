// Dijkstra in Rust, for the chapter "Graphs".
// 「グラフ」の章のダイクストラ法を、Rust で
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};

// ANCHOR: dijkstra
// Rust
fn dijkstra(graph: &HashMap<&str, Vec<(&str, u32)>>, start: &str)
    -> HashMap<String, u64>
{
    // The roads are u32 and the totals u64, so a total of fewer than
    // 2^32 roads always fits.
    // 道は u32、合計は u64。道が 2^32 本より少なければ、合計は収まる
    let mut done = HashMap::new();
    // BinaryHeap gives the largest first, so wrap in Reverse.
    // BinaryHeap は大きい順に出すので、Reverse で包んで小さい順にする
    let mut frontier = BinaryHeap::from([Reverse((0, start.to_string()))]);
    while let Some(Reverse((d, town))) = frontier.pop() {
        if done.contains_key(&town) {
            continue;
        }
        for &(next, h) in graph.get(town.as_str()).into_iter().flatten() {
            if !done.contains_key(next) {
                frontier.push(Reverse((d + u64::from(h), next.to_string())));
            }
        }
        done.insert(town, d);
    }
    done
}
// ANCHOR_END: dijkstra

fn main() {
    let roads = [("Start", "River", 2), ("Start", "Forest", 7),
                 ("River", "Forest", 3), ("Forest", "Castle", 4),
                 ("River", "Mountain", 6), ("Mountain", "Castle", 2),
                 ("Island", "Lighthouse", 1)];
    let mut graph: HashMap<&str, Vec<(&str, u32)>> = HashMap::new();
    for (a, b, h) in roads {
        graph.entry(a).or_default().push((b, h));
        graph.entry(b).or_default().push((a, h));
    }
    let dist = dijkstra(&graph, "Start");
    assert_eq!(dist["Castle"], 9);
    assert_eq!(dist["Mountain"], 8);
    assert!(!dist.contains_key("Island"));
    // A very long road does not overflow the total.
    // とても長い道があっても、合計はあふれない
    let mut long: HashMap<&str, Vec<(&str, u32)>> = HashMap::new();
    for (a, b, h) in [("S", "A", 1), ("A", "B", u32::MAX), ("S", "B", 3)] {
        long.entry(a).or_default().push((b, h));
        long.entry(b).or_default().push((a, h));
    }
    assert_eq!(dijkstra(&long, "S")["B"], 3);
    println!("ok");
}
