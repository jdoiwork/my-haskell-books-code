// Go: the sender forgot to close the channel (the same as deadlock.lam).
// The runtime finds that every goroutine is asleep, and stops.
// Go：送り手がチャネルを閉じ忘れた（deadlock.lam と同じ）。実行時システムが、
// どのゴルーチンも眠っていることを見つけて止める
package main

import "fmt"

// ANCHOR: deadlock
// Go
func main() {
	c := make(chan int)
	go func() {
		for _, n := range []int{1, 2, 3} {
			c <- n
		}
		// close(c) is forgotten / close(c) を忘れた
	}()
	for n := range c {
		fmt.Println("got", n)
	}
	fmt.Println("all done")
}
// ANCHOR_END: deadlock
