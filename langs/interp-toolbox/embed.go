// The site of the chapter, in Go: //go:embed puts public/ inside the
// program, and http.FileServerFS serves it, an image too.
// 章のサイトを Go で。//go:embed が public/ をプログラムの中に入れ、
// http.FileServerFS が、画像も見せる
package main

import (
	"bytes"
	"embed"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
)

// ANCHOR: embed
//go:embed public
var site embed.FS

func handler() http.Handler {
	return http.FileServerFS(site)
}

// ANCHOR_END: embed

func main() {
	server := httptest.NewServer(handler())
	defer server.Close()
	res, err := http.Get(server.URL + "/public/logo.png")
	if err != nil {
		panic(err)
	}
	got, _ := io.ReadAll(res.Body)
	want, _ := site.ReadFile("public/logo.png")
	if res.StatusCode != 200 || !bytes.Equal(got, want) {
		panic(fmt.Sprint("not the logo: ", res.StatusCode))
	}
	if kind := res.Header.Get("Content-Type"); kind != "image/png" {
		panic("not image/png: " + kind)
	}
	fmt.Println("ok")
}
