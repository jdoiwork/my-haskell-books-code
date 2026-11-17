// Go: channels between goroutines, the same as Lam's Chan: progress
// reports, a pool of workers, and select with a time limit.
// Go：ゴルーチンのあいだのチャネル。Lam の Chan と同じ形：進み具合の
// 知らせ、働き手の組、制限時間つきの select
package main

import (
	"fmt"
	"slices"
	"strings"
	"sync"
	"time"
)

var names = []string{"bulbasaur", "charmander", "squirtle", "pikachu"}

func fetch(name string) string {
	time.Sleep(10 * time.Millisecond)
	return strings.ToUpper(name)
}

func check(ok bool, what string) {
	if !ok {
		panic("failed: " + what)
	}
}

// ANCHOR: progress
// Go: send each answer as soon as it is ready, then close
// Go：答えができるたびに送り、最後に閉じる
func progress() []string {
	out := make(chan string) // like Chan.new 0
	go func() {
		for _, n := range names {
			out <- fetch(n)
		}
		close(out)
	}()
	got := []string{}
	for m := range out { // until it is closed
		got = append(got, m)
	}
	return got
}
// ANCHOR_END: progress

// ANCHOR: pool
// Go: three workers share one channel of names
// Go：3人の働き手が、名前のチャネル1本を分け合う
func pool() []string {
	todo := make(chan string)
	results := make(chan string)
	go func() {
		for _, n := range names {
			todo <- n
		}
		close(todo)
	}()
	var wg sync.WaitGroup
	for id := 1; id <= 3; id++ {
		wg.Go(func() {
			for n := range todo {
				results <- fmt.Sprintf("worker %d: %s", id, fetch(n))
			}
		})
	}
	// Close the results when all the workers are done.
	// 働き手が全員終わったら、答えを閉じる
	go func() {
		wg.Wait()
		close(results)
	}()
	got := []string{}
	for r := range results {
		got = append(got, r)
	}
	return got
}
// ANCHOR_END: pool

// ANCHOR: select
// Go: select takes whichever comes first; time.After is a time limit
// Go：select は先に来たほうを取る。time.After は制限時間
func firstOrGiveUp(slow time.Duration) string {
	answer := make(chan string, 1)
	go func() {
		time.Sleep(slow)
		answer <- "cake"
	}()
	select {
	case dish := <-answer:
		return "dessert: " + dish
	case <-time.After(50 * time.Millisecond):
		return "no cake yet: we go home"
	}
}
// ANCHOR_END: select

func main() {
	check(slices.Equal(progress(),
		[]string{"BULBASAUR", "CHARMANDER", "SQUIRTLE", "PIKACHU"}), "progress")
	got := pool()
	check(len(got) == 4, "pool")
	slices.Sort(got)
	check(strings.HasSuffix(got[0], "BULBASAUR") ||
		strings.Contains(strings.Join(got, ","), "BULBASAUR"), "pool names")
	check(firstOrGiveUp(10*time.Millisecond) == "dessert: cake", "fast")
	check(firstOrGiveUp(200*time.Millisecond) == "no cake yet: we go home", "slow")
	// A receive with nothing to come: Go finds it, too.
	// 何も来ない受け取り：Go も見つける（コメントだけ。動かすと止まる）
	fmt.Println("ok")
}
