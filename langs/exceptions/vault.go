// The Go code in the "IO exceptions" chapter.
// IO の例外をちゃんと扱うの章の Go のコード
package main

import (
	"errors"
	"fmt"
	"strings"
)

var log []string

var errGoblinAlarm = errors.New("goblin alarm")

func openDoor()  { log = append(log, "door: open") }
func closeDoor() { log = append(log, "door: closed") }

func countCoins() error {
	log = append(log, "counting: 1, 2, 3, ...")
	return errGoblinAlarm
}

// ANCHOR: defer
// Go (defer: run it when the function returns, whatever happens)
// Go（defer：関数を抜けるときに、何があっても動かす）
func nightShift() error {
	openDoor()
	defer closeDoor()
	if err := countCoins(); err != nil {
		return err
	}
	log = append(log, "counting: done")
	return nil
}

// ANCHOR_END: defer

func check(ok bool, what string) {
	if !ok {
		panic("failed: " + what)
	}
}

func main() {
	err := nightShift()
	check(errors.Is(err, errGoblinAlarm), "the alarm comes back")
	got := strings.Join(log, "|")
	check(got == "door: open|counting: 1, 2, 3, ...|door: closed", got)
	fmt.Println("ok")
}
