#!/bin/sh
# Make examples/to-js/<name>.out: each program run by lam, and the same
# program turned into JavaScript by lam to-js and run by node. Both must
# write the same (the standard output and error, and the exit code), or
# this stops. Also the JavaScript of shapes.lam without the runtime
# (shapes.js.out), and what lam to-js says for a program it cannot turn
# yet (hide.out). With --check, only compare.
# examples/to-js/<名前>.out を作る：lam で動かしたプログラムと、同じ
# プログラムを lam to-js で JavaScript にして node で動かしたもの。両方が
# 同じもの（標準出力と標準エラー、終了コード）を書かなければ止まる。
# ほかに、shapes.lam のランタイムを除いた JavaScript（shapes.js.out）と、
# まだ変換できないプログラムへの lam to-js の言葉（hide.out）。--check
# なら、くらべるだけ
set -eu
cd "$(dirname "$0")"
here=$(pwd)
LAMBIN=$(cabal list-bin -v0 exe:lam)
# Node 26 (notes/tooling.md), or the node on the PATH.
# Node 26（notes/tooling.md）か、PATH の node
NODE=${NODE:-/opt/homebrew/bin/node}
[ -x "$NODE" ] || NODE=node
check=no
[ "${1:-}" = --check ] && check=yes
status=0
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# 🖨️ Run a command in a folder: what it wrote, and [exit n] if not 0.
# フォルダーでコマンドを動かす：書いたものと、0 でなければ [exit n]
shown() {
  dir=$1
  shift
  set +e
  (cd "$dir" && "$@" < /dev/null > "$tmp/out.txt" 2>&1)
  code=$?
  set -e
  cat "$tmp/out.txt"
  [ "$code" = 0 ] || echo "[exit $code]"
}

# ⚖️ Keep (or compare) a file of examples/to-js.
# examples/to-js のファイルを残す（か、くらべる）
keep() {
  if [ "$check" = yes ]; then
    if ! cmp -s "$tmp/$1" "examples/to-js/$1"; then
      echo "different: examples/to-js/$1"
      diff "examples/to-js/$1" "$tmp/$1" || true
      status=1
    fi
  else
    cp "$tmp/$1" "examples/to-js/$1"
  fi
}

# ANCHOR: both
# 🔁 One program: lam and node must agree, and that is the .out.
# プログラム1つ：lam と node が同じでなければならず、それが .out
both() {
  name=$1
  dir=$2
  file=$3
  shift 3
  shown "$dir" "$LAMBIN" "$file" "$@" > "$tmp/$name.lam.txt"
  (cd "$dir" && "$LAMBIN" to-js "$file" -o "$tmp/$name.mjs")
  shown "$dir" "$NODE" "$tmp/$name.mjs" "$@" > "$tmp/$name.out"
  if ! cmp -s "$tmp/$name.lam.txt" "$tmp/$name.out"; then
    echo "lam and node differ: $name"
    diff "$tmp/$name.lam.txt" "$tmp/$name.out" || true
    status=1
  fi
  keep "$name.out"
}
# ANCHOR_END: both

both shapes examples/to-js shapes.lam
both numbers examples/to-js numbers.lam
both letters examples/to-js letters.lam
both together examples/to-js together.lam
both oops examples/to-js oops.lam
both deep examples/to-js deep.lam
both receipt examples/editor/receipt Receipt.lam
both wordfreq examples/script wordfreq.lam sample.txt

# The JavaScript of shapes.lam, from the line that makes the runtime.
# shapes.lam の JavaScript。ランタイムを作る行から
(cd examples/to-js && "$LAMBIN" to-js shapes.lam) \
  | sed -n '/^const \$rt = makeRuntime();/,$p' > "$tmp/shapes.js.out"
keep shapes.js.out

# A program with names the runtime does not have yet.
# ランタイムにまだない名前を使うプログラム
shown examples/regex "$LAMBIN" to-js hide.lam > "$tmp/hide.out"
keep hide.out
exit $status
