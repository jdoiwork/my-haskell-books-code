// Go: start a job with go, do something else, and wait for it later
// (spawn and await of Lam). Then many at once with sync.WaitGroup.
// Go：go で仕事を始め、ほかのことをして、あとで待つ（Lam の spawn と
// await）。それから、sync.WaitGroup でたくさんを同時に
package main

import (
	"fmt"
	"strings"
	"sync"
	"time"
)

var delays = map[string]time.Duration{
	"pikachu": 400 * time.Millisecond,
	"eevee":   300 * time.Millisecond,
	"snorlax": 600 * time.Millisecond,
}

func fetch(name string) string {
	time.Sleep(delays[name])
	return strings.ToUpper(name)
}

func check(ok bool, what string) {
	if !ok {
		panic("failed: " + what)
	}
}

// ANCHOR: spawn
// Go: spawn is go, and await is receiving from a channel
// Go：spawn は go、await はチャネルから受け取ること
func spawnAndAwait() (string, string) {
	task := make(chan string, 1)
	go func() { task <- fetch("snorlax") }()
	// Meanwhile, something else.
	// そのあいだに、ほかのこと
	party := "Pikachu, Eevee, Snorlax"
	return party, <-task
}
// ANCHOR_END: spawn

// ANCHOR: all
// Go: IO.all with a WaitGroup; each goroutine writes its own place
// Go：WaitGroup で IO.all。ゴルーチンはそれぞれ自分の場所に書く
func all(names []string) []string {
	results := make([]string, len(names))
	var wg sync.WaitGroup
	for i, name := range names {
		wg.Go(func() { results[i] = fetch(name) })
	}
	wg.Wait()
	return results
}
// ANCHOR_END: all

func main() {
	start := time.Now()
	party, mon := spawnAndAwait()
	check(party != "" && mon == "SNORLAX", "spawn")
	check(time.Since(start) < 700*time.Millisecond, "spawn time")

	start = time.Now()
	got := all([]string{"pikachu", "eevee", "snorlax"})
	took := time.Since(start)
	check(strings.Join(got, ",") == "PIKACHU,EEVEE,SNORLAX", "all")
	check(took >= 600*time.Millisecond && took < 800*time.Millisecond,
		"all at once: "+took.String())
	fmt.Println("ok")
}
