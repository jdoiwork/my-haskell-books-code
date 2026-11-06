// The sieve race: how long does a sieve up to n take? (the best of 3)
// ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
//   rustc -O race.rs && ./race 10000000
use std::time::Instant;

fn sieve(n: usize) -> usize {
    let mut is_prime = vec![true; n + 1];
    is_prime[0] = false;
    is_prime[1] = false;
    let mut p = 2;
    while p * p <= n {
        if is_prime[p] {
            let mut m = p * p;
            while m <= n {
                is_prime[m] = false;
                m += p;
            }
        }
        p += 1;
    }
    is_prime.iter().filter(|ok| **ok).count()
}

fn main() {
    let n: usize = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut best = f64::INFINITY;
    let mut count = 0;
    for _ in 0..3 {
        let start = Instant::now();
        count = sieve(std::hint::black_box(n));
        best = best.min(start.elapsed().as_secs_f64() * 1000.0);
    }
    println!("{} {:.1}", count, best);
}
