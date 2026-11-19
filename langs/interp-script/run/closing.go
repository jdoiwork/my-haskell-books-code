// The Go code in the chapter on scripts: os.Exit ends the program at
// once, and the deferred functions are NOT run.
// スクリプトの章の Go のコード：os.Exit はその場でプログラムを終える。
// defer した関数は動かない
package main

import (
	"fmt"
	"os"
)

// ANCHOR: go
// Go (os.Exit): the defer is skipped
// Go（os.Exit）：defer は飛ばされる
func main() {
	fmt.Println("the shop opens")
	defer fmt.Println("the shop closes")
	os.Exit(3)
}
// ANCHOR_END: go
