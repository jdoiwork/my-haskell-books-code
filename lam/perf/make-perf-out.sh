#!/bin/sh
# Make examples/perf/<name>.out: what each program of the chapter writes.
# Then check that every other way writes the same: lam with LAM_NO_FAST
# (only the Lam definitions), the Haskell programs of lam-direct, the
# programs written by hand in Python and TypeScript, and, for the
# programs that can be turned, lam to-js and lam to-python. The two log
# programs read a log of 2,000 lines, made with the seed 618. With
# --check, compare with the .out files and change nothing. Times are not
# checked: a busy machine changes them, not the answers.
# examples/perf/<名前>.out を作る：章のプログラムが書くもの。そのうえで、
# ほかのやり方も同じものを書くかを確かめる：LAM_NO_FAST の lam（Lam の
# 定義だけ）、lam-direct の Haskell のプログラム、Python と TypeScript で
# 手で書いたもの、変換できるものは lam to-js と lam to-python。ログの2つは、種 618 で作った 2,000 行のログを読む。--check なら
# .out とくらべ、何も変えない。時間は確かめない（忙しいマシンで変わるのは
# 時間で、答えではない）
set -eu
cd "$(dirname "$0")"
cabal build -v0 exe:lam exe:lam-direct
LAMBIN=$(cabal list-bin -v0 exe:lam)
DIRECT=$(cabal list-bin -v0 exe:lam-direct)
PYTHON=${PYTHON:-/opt/homebrew/bin/python3.15}
[ -x "$PYTHON" ] || PYTHON=python3
LANGS=../../langs/interp-performance
check=no
[ "${1:-}" = --check ] && check=yes
status=0
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
"$PYTHON" examples/perf/make-log.py 2000 "$tmp/small.log"

# ⚖️ Two ways must write the same.
# 2つのやり方は、同じものを書くはず
same() {
  if ! cmp -s "$1" "$2"; then
    echo "different: $3"
    diff "$1" "$2" || true
    status=1
  fi
}

for name in loop fib range sort deep count slow; do
  case $name in
    count|slow) file="$tmp/small.log" ;;
    *)          file="" ;;
  esac
  # shellcheck disable=SC2086
  (cd examples/perf && "$LAMBIN" $name.lam $file) > "$tmp/$name.out"
  if [ $check = yes ]; then
    same "examples/perf/$name.out" "$tmp/$name.out" "examples/perf/$name.out"
  else
    cp "$tmp/$name.out" "examples/perf/$name.out"
  fi
  # shellcheck disable=SC2086
  (cd examples/perf && LAM_NO_FAST=1 "$LAMBIN" $name.lam $file) \
    > "$tmp/$name.slow"
  same "$tmp/$name.out" "$tmp/$name.slow" "$name with LAM_NO_FAST"
  # shellcheck disable=SC2086
  "$DIRECT" $name $file > "$tmp/$name.direct"
  same "$tmp/$name.out" "$tmp/$name.direct" "$name of lam-direct"
  if (cd examples/perf && "$LAMBIN" to-js $name.lam -o "$tmp/$name.mjs") \
       2> /dev/null; then
    # shellcheck disable=SC2086
    node "$tmp/$name.mjs" $file > "$tmp/$name.node"
    same "$tmp/$name.out" "$tmp/$name.node" "$name in node"
    (cd examples/perf && "$LAMBIN" to-python $name.lam -o "$tmp/$name.py")
    # shellcheck disable=SC2086
    "$PYTHON" "$tmp/$name.py" $file > "$tmp/$name.python"
    same "$tmp/$name.out" "$tmp/$name.python" "$name in python3"
  fi
  # The same work written by hand in Python and TypeScript (the times of
  # bench.sh --python and --node).
  # 同じ仕事を Python と TypeScript で手で書いたもの（bench.sh の
  # --python と --node の時間）
  # shellcheck disable=SC2086
  "$PYTHON" "$LANGS/run/perf.py" $name $file > "$tmp/$name.byhand"
  same "$tmp/$name.out" "$tmp/$name.byhand" "$name of run/perf.py"
  # shellcheck disable=SC2086
  node --no-warnings "$LANGS/run/perf.ts" $name $file > "$tmp/$name.byhand"
  same "$tmp/$name.out" "$tmp/$name.byhand" "$name of run/perf.ts"
done

# The answer of step 3: one more program, the same with LAM_NO_FAST.
# ステップ3の解答：もう1つのプログラム。LAM_NO_FAST でも同じ
(cd examples/perf/answers && "$LAMBIN" ips.lam "$tmp/small.log") \
  > "$tmp/ips.out"
if [ $check = yes ]; then
  same examples/perf/answers/ips.out "$tmp/ips.out" examples/perf/answers/ips.out
else
  cp "$tmp/ips.out" examples/perf/answers/ips.out
fi
(cd examples/perf/answers && LAM_NO_FAST=1 "$LAMBIN" ips.lam "$tmp/small.log") \
  > "$tmp/ips.slow"
same "$tmp/ips.out" "$tmp/ips.slow" "answers/ips with LAM_NO_FAST"

# lam --count-calls: the counts are the same every time (they go to the
# standard error, after what the program writes).
# lam --count-calls：数は毎回同じ（プログラムが書くものの後に、標準エラーへ）
for name in sort count; do
  case $name in
    count) file="$tmp/small.log" ;;
    *)     file="" ;;
  esac
  for how in fast slow; do
    # shellcheck disable=SC2086
    if [ $how = slow ]; then
      (cd examples/perf && LAM_NO_FAST=1 "$LAMBIN" --count-calls $name.lam $file) \
        > /dev/null 2> "$tmp/calls"
    else
      (cd examples/perf && "$LAMBIN" --count-calls $name.lam $file) \
        > /dev/null 2> "$tmp/calls"
    fi
    out="examples/perf/calls-$name-$how.out"
    if [ $check = yes ]; then same "$out" "$tmp/calls" "$out"
    else cp "$tmp/calls" "$out"; fi
  done
done
exit $status
