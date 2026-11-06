// The Go code in the "server" chapter of the message broker part.
// メッセージブローカーの部の「サーバー」の章の Go のコード
package main

import (
	"bufio"
	"fmt"
	"net"
	"os"
	"strings"
)

// One mailbox, shared by every connection (competing consumers).
// 郵便受けは1つ。全部の接続で分け合う（受け手どうしで取り合う）
var jobs = make(chan string, 100)

// ANCHOR: server
// Go (a reader, one writer, and an outbox channel between them)
// Go（読み係と、ただ1人の書き係。そのあいだに outbox のチャネル）

// 🔌 Serve one connection.
// 1本の接続の相手をする
func handle(conn net.Conn) {
	outbox := make(chan string, 16)
	// The writer: the only one who writes to the socket.
	// 書き係：ソケットに書くのは、このゴルーチンだけ
	go func() {
		for line := range outbox {
			fmt.Fprint(conn, line+"\n")
		}
	}()
	// The reader: reads commands and puts answers into the outbox.
	// 読み係：コマンドを読み、答えを outbox に入れる
	lines := bufio.NewScanner(conn)
	for lines.Scan() {
		command, body, _ := strings.Cut(lines.Text(), " ")
		switch command {
		case "PUBLISH":
			jobs <- body
			outbox <- "OK"
		case "CONSUME":
			outbox <- "OK"
			// Deliver letters, through the same outbox.
			// 手紙を届ける。これも同じ outbox を通す
			go func() {
				for letter := range jobs {
					outbox <- "DELIVER " + letter
				}
			}()
		}
	}
}
// ANCHOR_END: server

// 🏠 Accept connections forever, one goroutine each.
// 接続を受け付け続ける。1本ごとにゴルーチンを1つ
func serve(listener net.Listener) {
	for {
		conn, err := listener.Accept()
		if err != nil {
			return
		}
		go handle(conn)
	}
}

func fail(what string) {
	fmt.Fprintln(os.Stderr, "failed:", what)
	os.Exit(1)
}

func main() {
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		fail(err.Error())
	}
	go serve(listener)
	addr := listener.Addr().String()

	publisher, err := net.Dial("tcp", addr)
	if err != nil {
		fail(err.Error())
	}
	pubLines := bufio.NewScanner(publisher)
	letters := []string{
		"A red dragon at the north gate",
		"Ten moon herbs, please",
		"A baby dragon is lost",
	}
	for _, letter := range letters {
		fmt.Fprint(publisher, "PUBLISH "+letter+"\n")
		if !pubLines.Scan() || pubLines.Text() != "OK" {
			fail("publish")
		}
	}

	consumer, err := net.Dial("tcp", addr)
	if err != nil {
		fail(err.Error())
	}
	conLines := bufio.NewScanner(consumer)
	fmt.Fprint(consumer, "CONSUME\n")
	if !conLines.Scan() || conLines.Text() != "OK" {
		fail("consume")
	}
	for _, letter := range letters {
		if !conLines.Scan() || conLines.Text() != "DELIVER "+letter {
			fail("deliver " + letter)
		}
	}
	fmt.Println("ok")
}
