// The Rust code in the "choosing a box" chapter.
// 書き換えられる箱の使い分けの章の Rust のコード
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc;
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::time::Duration;

// ANCHOR: once_lock
// Rust (OnceLock: a global that is filled once, the first time)
// Rust（OnceLock：最初の1回だけ中身を入れる、グローバルな箱）
static GREETING: OnceLock<String> = OnceLock::new();

fn greeting() -> &'static str {
    GREETING.get_or_init(|| String::from("Welcome to the guild!"))
}
// ANCHOR_END: once_lock

fn main() {
    // ANCHOR: cells
    // Rust (one thread: Cell for a number, RefCell for a Map)
    // Rust（スレッド1つ：数なら Cell、Map なら RefCell）
    let next = Cell::new(1);
    let take = || {
        let n = next.get();
        next.set(n + 1);
        n
    };
    let prices: RefCell<HashMap<&str, u32>> = RefCell::new(HashMap::new());
    prices.borrow_mut().insert("sword", 500);
    // ANCHOR_END: cells
    assert_eq!((take(), take()), (1, 2));
    assert_eq!(prices.borrow()["sword"], 500);

    // ANCHOR: atomic
    // Rust (many threads: an atomic number, and a Mutex for the Map)
    // Rust（スレッドがたくさん：数は Atomic、Map は Mutex に）
    let tickets = AtomicUsize::new(1);
    let cache: Mutex<HashMap<String, u32>> = Mutex::new(HashMap::new());
    thread::scope(|s| {
        for _ in 0..100 {
            s.spawn(|| {
                for _ in 0..10 {
                    tickets.fetch_add(1, Ordering::Relaxed);
                }
                cache.lock().unwrap().entry("sword".into()).or_insert(500);
            });
        }
    });
    // ANCHOR_END: atomic
    assert_eq!(tickets.load(Ordering::Relaxed), 1001);
    assert_eq!(cache.lock().unwrap().len(), 1);

    // ANCHOR: channel
    // Rust (a channel: when the sender is dropped, the clerks go home)
    // Rust（チャネル：送り手がいなくなったら、受付係は帰る）
    let (board, inbox) = mpsc::sync_channel::<String>(3);
    let inbox = Mutex::new(inbox);
    let done = Mutex::new(Vec::new());
    thread::scope(|s| {
        for me in 1..=3 {
            let (inbox, done) = (&inbox, &done);
            s.spawn(move || loop {
                let next = inbox.lock().unwrap().recv();
                match next {
                    Ok(quest) => {
                        thread::sleep(Duration::from_millis(10));
                        done.lock().unwrap().push((me, quest));
                    }
                    Err(_) => break, // closed / 閉店
                }
            });
        }
        for i in 1..=9 {
            board.send(format!("quest-{i}")).unwrap();
        }
        drop(board);
    });
    // ANCHOR_END: channel
    assert_eq!(done.lock().unwrap().len(), 9);

    assert_eq!(greeting(), "Welcome to the guild!");
    println!("ok");
}
