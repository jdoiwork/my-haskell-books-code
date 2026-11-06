// The Rust code in the "rewrite to go faster" chapter.
// 書き換えて速くするの章の Rust のコード

// ANCHOR: borrow
// Rust (only the one who holds &mut may rewrite; the caller gets a Vec)
// Rust（&mut を持つ人だけが書き換えられる。呼んだ人には Vec を返す）
fn cross_out(is_prime: &mut [bool], p: usize) {
    let mut m = p * p;
    while m < is_prime.len() {
        is_prime[m] = false;
        m += p;
    }
}

fn sieve(n: usize) -> Vec<bool> {
    let mut is_prime = vec![true; n + 1];
    is_prime[0] = false;
    is_prime[1] = false;
    let mut p = 2;
    while p * p <= n {
        if is_prime[p] {
            cross_out(&mut is_prime, p);
        }
        p += 1;
    }
    is_prime
}
// ANCHOR_END: borrow

fn main() {
    let primes: Vec<usize> = sieve(30)
        .iter()
        .enumerate()
        .filter(|(_, ok)| **ok)
        .map(|(i, _)| i)
        .collect();
    assert_eq!(primes, vec![2, 3, 5, 7, 11, 13, 17, 19, 23, 29]);
    assert_eq!(sieve(1000000).iter().filter(|ok| **ok).count(), 78498);
    println!("ok");
}
