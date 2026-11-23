#!/bin/sh
# ⏱️ Time the programs of examples/perf: each one is run once to warm up
#    (the first run of a new program is slow on macOS), then three
#    times, and the shortest time is shown (in seconds; "-" when that
#    way cannot run the program).
#      sh bench.sh                 this lam
#      sh bench.sh PATH/TO/lam     another lam (the one of to-python)
#      sh bench.sh --node          lam to-js, run by node
#      sh bench.sh --python        lam to-python, run by python3
#      sh bench.sh --direct        the same work, written in Haskell
#      sh bench.sh --by-hand       count and slow in Python and in awk
# examples/perf のプログラムの時間を測る。1回目は慣らし（macOS では、
# 新しいプログラムの初めの1回が遅い）。そのあと3回動かし、いちばん短い
# 時間を見せる（秒。そのやり方で動かせないものは「-」）
set -eu
cd "$(dirname "$0")/examples/perf"
here=$(pwd)
lam="$(cd ../.. && cabal list-bin -v0 exe:lam)"
langs="$here/../../../../langs/interp-performance"
python=/opt/homebrew/bin/python3.15
[ -x "$python" ] || python=python3
mode="${1:-$lam}"
[ -f big.log ] || "$python" make-log.py 200000 big.log
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# 🏃 The shortest of three runs, in seconds.
# 3回のうち、いちばん短い時間（秒）
best() {
  "$@" > /dev/null 2>&1
  for _ in 1 2 3; do
    /usr/bin/time -p "$@" 2>&1 > /dev/null | awk '/^real/ { print $2 }'
  done | sort -n | head -1
}

# 🔀 One program, one way: its time, or "-".
# 1つのプログラムを、1つのやり方で。時間か「-」
timed() {
  name=$1
  case $name in
    count|slow) file=big.log ;;
    *)          file="" ;;
  esac
  case $mode in
    --direct) best "$(cd ../.. && cabal list-bin -v0 exe:lam-direct)" \
                $name $file ;;
    --node)
      if "$lam" to-js "$name.lam" -o "$tmp/$name.mjs" 2> /dev/null; then
        best node "$tmp/$name.mjs" $file
      else echo "-"; fi ;;
    --python)
      if "$lam" to-python "$name.lam" -o "$tmp/$name.py" 2> /dev/null; then
        best "$python" "$tmp/$name.py" $file
      else echo "-"; fi ;;
    --by-hand)
      case $name in
        count) echo "$(best "$python" "$langs/count.py" big.log)" \
                    "$(best awk 'END { print NR }' big.log)" ;;
        slow)  echo "$(best "$python" "$langs/slow.py" big.log)" \
                    "$(best awk -f "$langs/run/slow.awk" big.log)" ;;
        *)     echo "-" ;;
      esac ;;
    *) best "$mode" "$name.lam" $file ;;
  esac
}

for name in loop fib range sort deep count slow; do
  printf '%-6s %s\n' "$name" "$(timed "$name")"
done
