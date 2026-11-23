# ANCHOR: all
# 🗣️ Talk to lam lsp as an editor would: start it, send it messages of
#    JSON-RPC with a Content-Length header, and show what it answers.
# エディターと同じように lam lsp と話す：起動して、Content-Length の
# ヘッダーを付けた JSON-RPC の知らせを送り、答えを見せる
#   python3 talk.py LAM FILE.lam LINE CHARACTER
import json
import os
import subprocess
import sys

lam, path, line, character = sys.argv[1:5]
uri = "file://" + os.path.abspath(path)
with open(path, encoding="utf-8") as f:
    text = f.read()

server = subprocess.Popen([lam, "lsp"], stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE)


# ANCHOR: send
# 📤 One message: a header, an empty line, then the bytes of the JSON.
# 知らせ1つ：ヘッダー、空の行、それから JSON のバイト
def send(message):
    body = json.dumps(message).encode("utf-8")
    server.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
    server.stdin.flush()


# 📥 One answer, read the same way.
# 答え1つ。同じ形で読む
def receive():
    size = 0
    while True:
        header = server.stdout.readline().decode("ascii").strip()
        if header == "":
            break
        if header.startswith("Content-Length:"):
            size = int(header.split(":")[1])
    return json.loads(server.stdout.read(size))
# ANCHOR_END: send


# 🖨️ An answer, with the long URI cut down to the name of the file.
# 答えを見せる。長い URI は、ファイルの名前だけにする
def show(answer):
    shown = json.dumps(answer, ensure_ascii=False, sort_keys=True)
    print("<-", shown.replace(uri, os.path.basename(path)))


# ANCHOR: talk
send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
      "params": {"capabilities": {}}})
show(receive())
send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
send({"jsonrpc": "2.0", "method": "textDocument/didOpen",
      "params": {"textDocument": {"uri": uri, "languageId": "lam",
                                  "version": 1, "text": text}}})
show(receive())
send({"jsonrpc": "2.0", "id": 2, "method": "textDocument/hover",
      "params": {"textDocument": {"uri": uri},
                 "position": {"line": int(line),
                              "character": int(character)}}})
show(receive())
send({"jsonrpc": "2.0", "id": 3, "method": "shutdown"})
show(receive())
send({"jsonrpc": "2.0", "method": "exit"})
print("exit code:", server.wait())
# ANCHOR_END: talk
# ANCHOR_END: all
