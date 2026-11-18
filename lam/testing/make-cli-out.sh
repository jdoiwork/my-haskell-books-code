#!/bin/sh
# Make examples/cli/<name>.out with the real lam, in a copy of the
# folder (so that todo.txt and the like stay out of the examples). Each
# line of <name>.cmd is a command; a line that starts with "@ " is run
# too, but its output is not kept (to get ready). With --check, only
# compare: the real lam must give what the .out files say.
# 本物の lam で examples/cli/<名前>.out を作る。todo.txt などが例に残らない
# よう、フォルダーの写しの中で動かす。<名前>.cmd の1行が1つのコマンド。
# 「@ 」で始まる行も動かすが、出力は残さない（下ごしらえ）。
# --check なら、くらべるだけ
set -eu
cd "$(dirname "$0")"
LAMBIN=$(cabal list-bin -v0 exe:lam)
export LAMBIN
check=no
[ "${1:-}" = --check ] && check=yes
status=0
for cmd in examples/cli/*.cmd; do
  name=${cmd%.cmd}
  tmp=$(mktemp -d)
  cp examples/cli/*.lam examples/cli/*.txt "$tmp/"
  : > "$tmp/shown.txt"
  while IFS= read -r line; do
    keep=yes
    case "$line" in "@ "*) keep=no; line=${line#@ } ;; esac
    set +e
    (cd "$tmp" && lam() { "$LAMBIN" "$@"; } && eval "$line" > out.txt 2>&1)
    code=$?
    set -e
    if [ "$keep" = yes ]; then
      cat "$tmp/out.txt" >> "$tmp/shown.txt"
      [ "$code" = 0 ] || echo "[exit $code]" >> "$tmp/shown.txt"
    fi
  done < "$cmd"
  if [ "$check" = yes ]; then
    if ! cmp -s "$tmp/shown.txt" "$name.out"; then
      echo "different: $name"
      diff "$name.out" "$tmp/shown.txt" || true
      status=1
    fi
  else
    cp "$tmp/shown.txt" "$name.out"
  fi
  rm -rf "$tmp"
done
exit $status
