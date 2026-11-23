#!/bin/sh
# ⏱️ Time the programs of examples/perf: each one is run once to warm up
#    (the first run of a new program is slow on macOS), then three
#    times, and the shortest time is shown (in seconds; "-" when that
#    way cannot run the program).
#      sh bench.sh                 this lam
#      sh bench.sh PATH/TO/lam     another lam (the one of to-python)
#      sh bench.sh --python        the same work, written by hand in Python
#      sh bench.sh --node          the same, in TypeScript, run by node
#      sh bench.sh --awk           count and slow, in awk
#      sh bench.sh --direct        the same work, written in Haskell
#    The other languages' programs are in code/langs/interp-performance/run/
#    (not turned from Lam: written as each language is usually written).
# examples/perf のプログラムの時間を測る。1回目は慣らし（macOS では、
# 新しいプログラムの初めの1回が遅い）。そのあと3回動かし、いちばん短い
# 時間を見せる（秒。そのやり方で動かせないものは「-」）。ほかの言語の
# プログラムは code/langs/interp-performance/run/ にある（Lam から変換した
# ものではなく、その言語でふつうに書いたもの）
set -eu
cd "$(dirname "$0")/examples/perf"
here=$(pwd)
lam="$(cd ../.. && cabal list-bin -v0 exe:lam)"
langs="$here/../../../../langs/interp-performance"
python=/opt/homebrew/bin/python3.15
[ -x "$python" ] || python=python3
mode="${1:-$lam}"
[ -f big.log ] || "$python" make-log.py 200000 big.log

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
    --python) best "$python" "$langs/run/perf.py" $name $file ;;
    --node)   best node --no-warnings "$langs/run/perf.ts" $name $file ;;
    --awk)
      case $name in
        count) best awk 'END { print NR }' big.log ;;
        slow)  best awk -f "$langs/run/slow.awk" big.log ;;
        *)     echo "-" ;;
      esac ;;
    *) best "$mode" "$name.lam" $file ;;
  esac
}

for name in loop fib range sort deep count slow; do
  printf '%-6s %s\n' "$name" "$(timed "$name")"
done
