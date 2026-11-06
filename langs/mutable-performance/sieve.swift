// The Swift code in the "rewrite to go faster" chapter.
// 書き換えて速くするの章の Swift のコード

// ANCHOR: array
// Swift (a var array of Bools, rewritten in place)
// Swift（var の Bool の配列を、その場で書き換える）
func sieve(_ n: Int) -> [Bool] {
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
    return isPrime
}
// ANCHOR_END: array

// ANCHOR: cow
// Swift (copy on write: let b = a shares, and b copies when it changes)
// Swift（書くときにコピー：b = a では共有し、b を変えるときにコピーする）
let a = sieve(10)
var b = a
b[0] = true
assert(a[0] == false && b[0] == true)
// ANCHOR_END: cow

let primes = sieve(30).indices.filter { sieve(30)[$0] }
assert(primes == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29])
assert(sieve(1000000).filter { $0 }.count == 78498)
print("ok")
