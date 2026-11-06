// The Rust code in the "lifting IO" chapter.
// IO を持ち上げるの章の Rust のコード
use std::future::Future;
use std::pin::pin;
use std::sync::Arc;
use std::task::{Context, Poll, Wake, Waker};
use std::thread::{self, Thread};

// ANCHOR: block_on
// Rust (a tiny block_on: run a Future from a plain function, and wait)
// Rust（小さな block_on：ふつうの関数から Future を動かして、待つ）
struct ThreadWaker(Thread);

impl Wake for ThreadWaker {
    fn wake(self: Arc<Self>) {
        self.0.unpark();
    }
}

fn block_on<F: Future>(future: F) -> F::Output {
    let mut future = pin!(future);
    let waker = Waker::from(Arc::new(ThreadWaker(thread::current())));
    let mut cx = Context::from_waker(&waker);
    loop {
        match future.as_mut().poll(&mut cx) {
            Poll::Ready(value) => return value,
            // Sleep until someone calls wake. / wake されるまで眠る
            Poll::Pending => thread::park(),
        }
    }
}
// ANCHOR_END: block_on

// ANCHOR: report
// Rust (async inside; block_on is the runner, like runApp env)
// Rust（中は async。block_on がランナー。runApp env のようなもの）
async fn appraise(item: &str) -> usize {
    item.len() * 100
}

async fn appraise_all(items: &[&str]) -> Vec<usize> {
    let mut prices = Vec::new();
    for item in items {
        prices.push(appraise(item).await);
    }
    prices
}

fn closing_report(items: &[&str]) -> usize {
    // let prices = appraise_all(items).await;  // error outside async
    let prices = block_on(appraise_all(items));
    prices.iter().sum()
}
// ANCHOR_END: report

fn main() {
    assert_eq!(closing_report(&["sword", "shield", "herb"]), 1500);
    assert_eq!(block_on(appraise_all(&["herb"])), vec![400]);
    println!("ok");
}
