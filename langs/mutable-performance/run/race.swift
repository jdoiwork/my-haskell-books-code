// The sieve race: how long does a sieve up to n take? (the best of 3)
// ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
//   swiftc -O -o race race.swift && ./race 10000000
import Foundation

func sieve(_ n: Int) -> Int {
    var isPrime = [Bool](repeating: true, count: n + 1)
    isPrime[0] = false
    isPrime[1] = false
    var p = 2
    while p * p <= n {
        if isPrime[p] {
            for m in stride(from: p * p, through: n, by: p) {
                isPrime[m] = false
            }
        }
        p += 1
    }
    var count = 0
    for ok in isPrime where ok { count += 1 }
    return count
}

let n = Int(CommandLine.arguments[1])!
var best = Double.infinity
var count = 0
for _ in 0..<3 {
    let start = DispatchTime.now().uptimeNanoseconds
    count = sieve(n)
    let ms = Double(DispatchTime.now().uptimeNanoseconds - start) / 1e6
    best = min(best, ms)
}
print(count, String(format: "%.1f", best))
