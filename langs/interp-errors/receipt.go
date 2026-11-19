// The Go code in the chapter on ? and cleaning up: an error is a
// value, looked at with if err != nil; defer runs when the function
// ends (the last defer first), even after a panic.
// ? と後始末の章の Go のコード：エラーは値で、if err != nil で見る。
// defer は関数が終わるときに動く（後の defer から）。panic のあとでも
package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// ANCHOR: iferr
// Go (if err != nil): look at each error, and give it back
// Go（if err != nil）：エラーを1つずつ見て、返す
func total(read func(string) (string, error), log *[]string) (int, error) {
	*log = append(*log, "the shop opens")
	defer func() { *log = append(*log, "the shop closes") }()
	text, err := read("orders.txt")
	if err != nil {
		return 0, err
	}
	sum := 0
	for _, word := range strings.Fields(text) {
		n, err := strconv.Atoi(word)
		if err != nil {
			return 0, fmt.Errorf("a bad number: %w", err)
		}
		sum += n
	}
	return sum, nil
}
// ANCHOR_END: iferr

// ANCHOR: defer
// Go (defer): runs when the function ends, the last one first;
// a defer in a loop waits for the end of the function, not of the loop
// Go（defer）：関数の終わりに、後のものから。ループの中の defer も、
// ループではなく関数の終わりまで待つ
func steps(log *[]string) {
	for i := 1; i <= 2; i++ {
		defer func() { *log = append(*log, fmt.Sprintf("defer %d", i)) }()
		*log = append(*log, fmt.Sprintf("loop %d", i))
	}
	*log = append(*log, "body ends")
}
// ANCHOR_END: defer

// A panic stops the function, but the defers still run.
// panic は関数を止めるが、defer は動く
func stopped(log *[]string) (said string) {
	defer func() {
		if r := recover(); r != nil {
			said = fmt.Sprint(r)
		}
	}()
	defer func() { *log = append(*log, "cleanup") }()
	var counts []int
	_ = counts[3] // index out of range: a panic
	return "never"
}

func main() {
	read := func(name string) (string, error) {
		if name == "orders.txt" {
			return "4 1 3", nil
		}
		return "", errors.New(name + ": no such file")
	}
	var log []string
	sum, err := total(read, &log)
	if sum != 8 || err != nil {
		panic("total")
	}
	missing := func(name string) (string, error) {
		return "", errors.New(name + ": no such file")
	}
	_, err = total(missing, &log)
	if err == nil || err.Error() != "orders.txt: no such file" {
		panic("missing")
	}
	broken := func(string) (string, error) { return "4 one", nil }
	_, err = total(broken, &log)
	var numErr *strconv.NumError
	if !errors.As(err, &numErr) {
		panic("broken: the NumError is still inside")
	}
	want := "the shop opens,the shop closes,the shop opens,the shop closes," +
		"the shop opens,the shop closes"
	if strings.Join(log, ",") != want {
		panic(strings.Join(log, ","))
	}

	var order []string
	steps(&order)
	if strings.Join(order, ",") != "loop 1,loop 2,body ends,defer 2,defer 1" {
		panic(strings.Join(order, ","))
	}

	var after []string
	said := stopped(&after)
	if !strings.Contains(said, "index out of range") || after[0] != "cleanup" {
		panic(said)
	}
	fmt.Println("ok receipt.go")
}
