// The Go code in the "server" chapter of the message broker part.
// メッセージブローカーの部の「サーバー」の章の Go のコード
package main

import (
	"bufio"
	"context"
	"fmt"
	"net"
	"os"
	"strings"
	"sync"
	"time"
)

// One mailbox, shared by every connection (competing consumers).
// 郵便受けは1つ。全部の接続で分け合う（受け手どうしで取り合う）
var jobs = make(chan string, 100)

// ANCHOR: server
// Go (a reader, one writer, and an outbox channel between them)
// Go（読み係と、ただ1人の書き係。そのあいだに outbox のチャネル）

// 🔌 Serve one connection. When the client hangs up, the consumers
// stop first, then the writer says what is left, and then we close.
// 1本の接続の相手をする。客が切ったら、まず受け手が止まり、
// 書き係が残りを言い終えてから、閉じる
func handle(conn net.Conn) {
	defer conn.Close()
	// ctx is cancelled when this connection ends.
	// ctx は、この接続が終わるときに取り消される
	ctx, stop := context.WithCancel(context.Background())
	outbox := make(chan string, 16)
	var consumers sync.WaitGroup

	// The writer: the only one who writes to the socket.
	// 書き係：ソケットに書くのは、このゴルーチンだけ
	written := make(chan struct{})
	go func() {
		defer close(written)
		for line := range outbox {
			if _, err := fmt.Fprint(conn, line+"\n"); err != nil {
				// The client is gone: stop everyone, wake the reader.
				// 客がいない。みんなを止め、読み係を起こす
				stop()
				conn.Close()
				return
			}
		}
	}()

	// Put a line in the outbox, unless the connection is ending.
	// 行を outbox に入れる。ただし、接続が終わるところなら、やめる
	say := func(line string) bool {
		select {
		case outbox <- line:
			return true
		case <-ctx.Done():
			return false
		}
	}

	// The reader: reads commands and puts answers into the outbox.
	// 読み係：コマンドを読み、答えを outbox に入れる
	lines := bufio.NewScanner(conn)
	for lines.Scan() {
		command, body, _ := strings.Cut(lines.Text(), " ")
		switch command {
		case "PUBLISH":
			jobs <- body
			say("OK")
		case "CONSUME":
			say("OK")
			consumers.Add(1)
			// Deliver letters through the same outbox, until the end.
			// 手紙を届ける。これも同じ outbox を通す。終わりまで
			go func() {
				defer consumers.Done()
				for {
					select {
					case letter := <-jobs:
						// If say gives up here, the letter is lost.
						// ここで say があきらめると、手紙は消える
						if !say("DELIVER " + letter) {
							return
						}
					case <-ctx.Done():
						return
					}
				}
			}()
		}
	}
	// The client hung up: stop the consumers, and wait for them.
	// 客が切った。受け手を止めて、止まるまで待つ
	stop()
	consumers.Wait()
	// The last words: the writer says what is left, and ends.
	// 最後の言葉：書き係が残りを言い終えて、終わる
	close(outbox)
	<-written
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

	// A consumer that hangs up takes no more letters: every letter
	// sent after that goes to the next consumer.
	// 切った受け手は、もう手紙を取らない。そのあとに送った手紙は、
	// 全部、次の受け手に届く
	consumer.Close()
	time.Sleep(100 * time.Millisecond)
	later := []string{"Wolves in the west", "A lost cat", "Pick herbs"}
	for _, letter := range later {
		fmt.Fprint(publisher, "PUBLISH "+letter+"\n")
		if !pubLines.Scan() || pubLines.Text() != "OK" {
			fail("publish")
		}
	}
	next, err := net.Dial("tcp", addr)
	if err != nil {
		fail(err.Error())
	}
	nextLines := bufio.NewScanner(next)
	fmt.Fprint(next, "CONSUME\n")
	if !nextLines.Scan() || nextLines.Text() != "OK" {
		fail("consume again")
	}
	for _, letter := range later {
		if !nextLines.Scan() || nextLines.Text() != "DELIVER "+letter {
			fail("deliver after hang-up " + letter)
		}
	}
	fmt.Println("ok")
}
