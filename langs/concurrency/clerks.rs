// The Rust code in the concurrency chapter.
// 並行・並列の章の Rust のコード
use std::sync::Mutex;
use std::thread;

fn main() {
    // ANCHOR: mutex
    // Rust (threads and a Mutex: the count can only be touched while locked)
    // Rust（スレッドと Mutex：錠をかけている間しか、数にさわれない）
    let count = Mutex::new(0);
    thread::scope(|s| {
        for _ in 0..10 {
            s.spawn(|| {
                for _ in 0..100_000 {
                    *count.lock().unwrap() += 1;
                }
            });
        }
    });
    // ANCHOR_END: mutex
    assert_eq!(*count.lock().unwrap(), 1_000_000);
    println!("ok");
}
