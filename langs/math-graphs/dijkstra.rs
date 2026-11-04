// Dijkstra in Rust, for the chapter "Graphs".
// 「グラフ」の章のダイクストラ法を、Rust で
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};

// ANCHOR: dijkstra
// Rust
fn dijkstra(graph: &HashMap<&str, Vec<(&str, u32)>>, start: &str)
    -> HashMap<String, u32>
{
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
                frontier.push(Reverse((d + h, next.to_string())));
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
                 ("River", "Mountain", 6), ("Mountain", "Castle", 2)];
    let mut graph: HashMap<&str, Vec<(&str, u32)>> = HashMap::new();
    for (a, b, h) in roads {
        graph.entry(a).or_default().push((b, h));
        graph.entry(b).or_default().push((a, h));
    }
    let dist = dijkstra(&graph, "Start");
    assert_eq!(dist["Castle"], 9);
    assert_eq!(dist["Mountain"], 8);
    println!("ok");
}
