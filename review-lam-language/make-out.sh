#!/bin/sh
# Make report/<name>.out with the real lam of lam/modules. <name>.lam is
# run as `lam <name>.lam`; <name>.txt is typed into the REPL line by
# line, and the .out shows each line after "lam> ", with its answer
# below it (as the book shows a REPL). With --check, only compare.
# lam/modules の本物の lam で report/<名前>.out を作る。<名前>.lam は
# `lam <名前>.lam` で動かす。<名前>.txt は REPL に1行ずつ打ち、.out には
# 「lam> 」のあとに打った行、その下に答えを並べる（本が REPL を見せる形）。
# --check なら、くらべるだけ
set -eu
cd "$(dirname "$0")/report"
LAMBIN=$(cd ../../lam/modules && cabal list-bin -v0 exe:lam)
check=no
[ "${1:-}" = --check ] && check=yes
status=0
tmp=$(mktemp)
for input in report.lam *.txt; do
  name=${input%.*}
  case "$input" in
    *.lam) "$LAMBIN" "$input" > "$tmp" 2>&1 || echo "[exit $?]" >> "$tmp" ;;
    *.txt) "$LAMBIN" < "$input" 2>&1 | python3 -c '
import sys
typed = open(sys.argv[1], encoding="utf-8").read().splitlines()
said = sys.stdin.read().split("lam> ")[1:]
for line, answer in zip(typed, said):
    print("lam> " + line)
    if answer.strip():
        print(answer.rstrip("\n"))
' "$input" > "$tmp" ;;
  esac
  if [ "$check" = yes ]; then
    if ! cmp -s "$tmp" "$name.out"; then
      echo "different: report/$name.out"
      diff "$name.out" "$tmp" || true
      status=1
    fi
  else
    cp "$tmp" "$name.out"
  fi
done
rm -f "$tmp"
exit $status
