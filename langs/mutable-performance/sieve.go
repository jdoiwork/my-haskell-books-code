// The Go code in the "rewrite to go faster" chapter.
// 書き換えて速くするの章の Go のコード
package main

import (
	"fmt"
	"slices"
)

// ANCHOR: slice
// Go (a slice of bools, rewritten in place)
// Go（bool のスライスを、その場で書き換える）
func sieve(n int) []bool {
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
	return isPrime
}
// ANCHOR_END: slice

func check(ok bool, what string) {
	if !ok {
		panic("failed: " + what)
	}
}

func main() {
	var primes []int
	for i, ok := range sieve(30) {
		if ok {
			primes = append(primes, i)
		}
	}
	check(slices.Equal(primes, []int{2, 3, 5, 7, 11, 13, 17, 19, 23, 29}),
		"primes")
	count := 0
	for _, ok := range sieve(1000000) {
		if ok {
			count++
		}
	}
	check(count == 78498, "count")
	fmt.Println("ok")
}
