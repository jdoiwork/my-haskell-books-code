// The Go code in the "lifting IO" chapter.
// IO を持ち上げるの章の Go のコード
package main

import (
	"fmt"
	"slices"
	"sync"
	"time"
)

// ANCHOR: nocolor
// Go (no colors: any plain function can wait, and start goroutines)
// Go（色がない：ふつうの関数のまま待てるし、goroutine も始められる）
func appraise(item string) int {
	time.Sleep(100 * time.Millisecond)
	return len(item) * 100
}

func appraiseAll(items []string) []int {
	prices := make([]int, len(items))
	var wg sync.WaitGroup
	for i, item := range items {
		wg.Go(func() { prices[i] = appraise(item) })
	}
	wg.Wait()
	return prices
}

func closingReport(items []string) int {
	total := 0
	for _, price := range appraiseAll(items) {
		total += price
	}
	return total
}
// ANCHOR_END: nocolor

func main() {
	start := time.Now()
	if closingReport([]string{"sword", "shield", "herb"}) != 1500 {
		panic("total")
	}
	if time.Since(start) > 250*time.Millisecond {
		panic("not at the same time")
	}
	if !slices.Equal(appraiseAll([]string{"herb"}), []int{400}) {
		panic("herb")
	}
	fmt.Println("ok")
}
