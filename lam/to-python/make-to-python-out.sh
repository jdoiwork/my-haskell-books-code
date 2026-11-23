#!/bin/sh
# Make examples/to-python/<name>.out: each program run by lam, and the
# same program turned into Python by lam to-python and run by python3.
# Both must write the same (the standard output and error, and the exit
# code), or this stops. Also the Python of shapes.lam and closures.lam
# without the runtime (shapes.py.out, closures.py.out), and what lam
# to-python says for a program it cannot turn yet (hide.out). With
# --check, only compare.
# examples/to-python/<名前>.out を作る：lam で動かしたプログラムと、同じ
# プログラムを lam to-python で Python にして python3 で動かしたもの。両方が
# 同じもの（標準出力と標準エラー、終了コード）を書かなければ止まる。
# ほかに、shapes.lam と closures.lam のランタイムを除いた Python
# （shapes.py.out・closures.py.out）と、まだ変換できないプログラムへの
# lam to-python の言葉（hide.out）。--check なら、くらべるだけ
set -eu
cd "$(dirname "$0")"
LAMBIN=$(cabal list-bin -v0 exe:lam)
# Python 3.15 (notes/tooling.md), or the python3 on the PATH.
# Python 3.15（notes/tooling.md）か、PATH の python3
PYTHON=${PYTHON:-/opt/homebrew/bin/python3.15}
[ -x "$PYTHON" ] || PYTHON=python3
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

# ⚖️ Keep (or compare) a file of examples/to-python.
# examples/to-python のファイルを残す（か、くらべる）
keep() {
  if [ "$check" = yes ]; then
    if ! cmp -s "$tmp/$1" "examples/to-python/$1"; then
      echo "different: examples/to-python/$1"
      diff "examples/to-python/$1" "$tmp/$1" || true
      status=1
    fi
  else
    cp "$tmp/$1" "examples/to-python/$1"
  fi
}

# ANCHOR: both
# 🔁 One program: lam and python3 must agree, and that is the .out.
#    python3 -I: the folder of the program is not put on the path of
#    the modules, so a file there cannot stand for a module it imports.
# プログラム1つ：lam と python3 が同じでなければならず、それが .out。
# python3 -I：プログラムのフォルダーを、モジュールを探す道に入れない。
# そこにあるファイルが、import するモジュールのふりをしないように
both() {
  name=$1
  dir=$2
  file=$3
  shift 3
  shown "$dir" "$LAMBIN" "$file" "$@" > "$tmp/$name.lam.txt"
  (cd "$dir" && "$LAMBIN" to-python "$file" -o "$tmp/$name.py")
  shown "$dir" "$PYTHON" -I "$tmp/$name.py" "$@" > "$tmp/$name.out"
  if ! cmp -s "$tmp/$name.lam.txt" "$tmp/$name.out"; then
    echo "lam and python3 differ: $name"
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
both floats examples/to-python floats.lam
both closures examples/to-python closures.lam
both depth examples/to-python depth.lam
both receipt examples/editor/receipt Receipt.lam
both wordfreq examples/script wordfreq.lam sample.txt

# The Python of a program, from the line that makes the runtime.
# プログラムの Python。ランタイムを作る行から
program() {
  (cd "$1" && "$LAMBIN" to-python "$2") \
    | sed -n '/^rt = make_runtime()$/,$p' > "$tmp/$3"
  keep "$3"
}
program examples/to-js shapes.lam shapes.py.out
program examples/to-python closures.lam closures.py.out

# closures.lam without the default arguments n=n: each function sees
# the n of the end of the loop (closures-late.out).
# closures.lam から既定の引数 n=n を取ると、どの関数も、ループが終わった
# ときの n を見る（closures-late.out）
(cd examples/to-python && "$LAMBIN" to-python closures.lam) \
  | sed 's/, n=n//' > "$tmp/late.py"
"$PYTHON" -I "$tmp/late.py" > "$tmp/closures-late.out"
keep closures-late.out

# A program with names the runtime does not have yet.
# ランタイムにまだない名前を使うプログラム
shown examples/regex "$LAMBIN" to-python hide.lam > "$tmp/hide.out"
keep hide.out
exit $status
