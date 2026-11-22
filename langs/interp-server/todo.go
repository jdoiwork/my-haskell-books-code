// The TODO API of the chapter, in Go: ServeMux with the patterns of Go
// 1.22 ("GET /todos/{id}"). Each connection is a goroutine of its own,
// so the list needs a mutex.
// 章の TODO の API を Go で。Go 1.22 からの ServeMux のパターン
// （"GET /todos/{id}"）を使う。接続ごとに goroutine なので、一覧には
// ミューテックスが要る
package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strconv"
	"strings"
	"sync"
)

// ANCHOR: store
type Todo struct {
	ID    int    `json:"id"`
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

type Store struct {
	mu    sync.Mutex
	todos []Todo
}

// ANCHOR_END: store

// ANCHOR: mux
func (s *Store) routes() *http.ServeMux {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /todos", s.listAll)
	mux.HandleFunc("POST /todos", s.addOne)
	mux.HandleFunc("PUT /todos/{id}/done", s.finishOne)
	return mux
}

type W = http.ResponseWriter
type R = *http.Request

func (s *Store) listAll(w W, r R) {
	s.mu.Lock()
	defer s.mu.Unlock()
	reply(w, 200, s.todos)
}

func (s *Store) addOne(w W, r R) {
	var body struct{ Title string }
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		http.Error(w, "bad request", 400)
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	todo := Todo{ID: len(s.todos) + 1, Title: body.Title}
	s.todos = append(s.todos, todo)
	reply(w, 201, todo)
}

func (s *Store) finishOne(w W, r R) {
	id, err := strconv.Atoi(r.PathValue("id"))
	if err != nil {
		http.Error(w, "the id is not a number", 400)
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	for i := range s.todos {
		if s.todos[i].ID == id {
			s.todos[i].Done = true
			reply(w, 200, s.todos[i])
			return
		}
	}
	http.NotFound(w, r)
}

// ANCHOR_END: mux

func reply(w http.ResponseWriter, status int, v any) {
	w.Header().Set("content-type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(v)
}

func main() {
	// A real server on a free port, only for the check.
	// 確かめのためだけの、空いているポートの本物のサーバー
	server := httptest.NewServer((&Store{}).routes())
	defer server.Close()
	var wg sync.WaitGroup
	for i := range 50 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			body := strings.NewReader(fmt.Sprintf(`{"title": "job %d"}`, i))
			r, err := http.Post(server.URL+"/todos", "application/json", body)
			if err != nil || r.StatusCode != 201 {
				panic("POST failed")
			}
			r.Body.Close()
		}()
	}
	wg.Wait()
	r, _ := http.Get(server.URL + "/todos")
	var all []Todo
	json.NewDecoder(r.Body).Decode(&all)
	seen := map[int]bool{}
	for _, t := range all {
		seen[t.ID] = true
	}
	if len(all) != 50 || len(seen) != 50 || !seen[1] || !seen[50] {
		panic(fmt.Sprint("the numbers are wrong: ", len(all), len(seen)))
	}
	// 405 for a method the path does not have, with Allow.
	// 道にないメソッドは 405。Allow も付く
	req, _ := http.NewRequest("PATCH", server.URL+"/todos/1/done", nil)
	r, _ = http.DefaultClient.Do(req)
	if r.StatusCode != 405 || r.Header.Get("Allow") != "PUT" {
		panic(fmt.Sprint("not 405: ", r.StatusCode, r.Header.Get("Allow")))
	}
	fmt.Println("ok")
}
