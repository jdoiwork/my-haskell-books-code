// The Go code in the "choosing a box" chapter.
// 書き換えられる箱の使い分けの章の Go のコード
package main

import (
	"fmt"
	"slices"
	"sync"
	"time"
)

// ANCHOR: channel
// Go (a channel with room for 3; close() is the closing sign)
// Go（3枚まで入るチャネル。close() が閉店の合図）
func guildDay(clerks int, quests []string) []string {
	board := make(chan string, 3)
	var mu sync.Mutex
	var done []string
	var wg sync.WaitGroup
	for me := 1; me <= clerks; me++ {
		wg.Go(func() {
			for quest := range board { // ends when closed / 閉店で終わる
				time.Sleep(10 * time.Millisecond)
				mu.Lock()
				done = append(done, quest)
				mu.Unlock()
			}
		})
	}
	for _, q := range quests {
		board <- q
	}
	close(board)
	wg.Wait()
	return done
}
// ANCHOR_END: channel

// ANCHOR: once
// Go (sync.Once: do it the first time only)
// Go（sync.Once：最初の1回だけする）
var (
	greetingOnce sync.Once
	greeting     string
)

func getGreeting() string {
	greetingOnce.Do(func() { greeting = "Welcome to the guild!" })
	return greeting
}
// ANCHOR_END: once

func check(ok bool, what string) {
	if !ok {
		panic("failed: " + what)
	}
}

func main() {
	quests := []string{}
	for i := 1; i <= 9; i++ {
		quests = append(quests, fmt.Sprintf("quest-%d", i))
	}
	done := guildDay(3, quests)
	slices.Sort(done)
	check(slices.Equal(done, quests), "every quest")
	check(getGreeting() == "Welcome to the guild!", "greeting")
	fmt.Println("ok")
}
