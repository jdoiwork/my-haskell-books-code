// The sieve race: how long does a sieve up to n take? (the best of 3)
// ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
//   go run race.go 10000000
package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"time"
)

func sieve(n int) int {
	isPrime := make([]bool, n+1)
	for i := 2; i <= n; i++ {
		isPrime[i] = true
	}
	for p := 2; p*p <= n; p++ {
		if isPrime[p] {
			for m := p * p; m <= n; m += p {
				isPrime[m] = false
			}
		}
	}
	count := 0
	for _, ok := range isPrime {
		if ok {
			count++
		}
	}
	return count
}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	best := math.Inf(1)
	count := 0
	for i := 0; i < 3; i++ {
		start := time.Now()
		count = sieve(n)
		best = math.Min(best, float64(time.Since(start).Microseconds())/1000)
	}
	fmt.Printf("%d %.1f\n", count, best)
}
