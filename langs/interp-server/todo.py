# The TODO API of the chapter, with Python's standard library only.
# ThreadingHTTPServer answers each request on a thread of its own, so
# the list needs a lock (Lam gives it to one actor instead).
# 章の TODO の API を、Python の標準ライブラリだけで。ThreadingHTTPServer は
# 頼みごとごとにスレッドを使うので、一覧には鍵が要る（Lam はアクター1人に
# 持たせる）
import json
import threading
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
todos: list[dict] = []
lock = threading.Lock()


class TodoHandler(BaseHTTPRequestHandler):
    def answer(self, status: int, body: object) -> None:
        data = json.dumps(body, ensure_ascii=False).encode()
        self.send_response(status)
        self.send_header("content-type", "application/json")
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self) -> None:
        if self.path == "/todos":
            with lock:
                self.answer(200, todos)
        else:
            self.answer(404, {"error": "not found"})

    def do_POST(self) -> None:
        if self.path != "/todos":
            self.answer(404, {"error": "not found"})
            return
        size = int(self.headers["content-length"])
        title = json.loads(self.rfile.read(size))["title"]
        with lock:
            todo = {"id": len(todos) + 1, "title": title, "done": False}
            todos.append(todo)
        self.answer(201, todo)

    def log_message(self, format: str, *args: object) -> None:
        pass  # quiet in the checks / 確かめでは黙っておく
# ANCHOR_END: procedural


# ANCHOR: routes
# Python (functional style): a table from (method, path) to a function
# Python（関数型スタイル）：(メソッド, 道) から関数への表
def list_all(body: dict | None) -> tuple[int, object]:
    with lock:
        return 200, list(todos)


def add_one(body: dict | None) -> tuple[int, object]:
    with lock:
        todo = {"id": len(todos) + 1, "title": body["title"], "done": False}
        todos.append(todo)
    return 201, todo


routes = {("GET", "/todos"): list_all, ("POST", "/todos"): add_one}
# ANCHOR_END: routes


# Start on a free port (0), ask it from the same program, and stop it.
# 空いているポート（0）で始め、同じプログラムから頼み、止める
server = ThreadingHTTPServer(("127.0.0.1", 0), TodoHandler)
threading.Thread(target=server.serve_forever, daemon=True).start()
url = f"http://127.0.0.1:{server.server_address[1]}/todos"


def post(i: int) -> dict:
    data = json.dumps({"title": f"job {i}"}).encode()
    with urllib.request.urlopen(urllib.request.Request(url, data)) as r:
        assert r.status == 201
        return json.load(r)


# 50 POSTs, 10 at once: the lock keeps the numbers apart.
# 50 回の POST を 10 ずつ同時に。鍵のおかげで番号がかぶらない
with ThreadPoolExecutor(10) as pool:
    made = list(pool.map(post, range(50)))
assert sorted(t["id"] for t in made) == list(range(1, 51))
with urllib.request.urlopen(url) as r:
    assert len(json.load(r)) == 50
server.shutdown()

# The table gives the same answers, as functions we can call alone.
# 表の関数も、同じ答え。関数だけで呼べる
todos.clear()
assert routes[("POST", "/todos")]({"title": "a"}) == (
    201, {"id": 1, "title": "a", "done": False})
assert routes[("GET", "/todos")](None)[1] == [
    {"id": 1, "title": "a", "done": False}]
print("ok")
