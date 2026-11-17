#!/bin/sh
# Make examples/oneliner/<name>.out with the real lam, in a copy of the
# folder (so that -i does not change the examples). With --check, only
# compare: the real lam must give what the .out files say.
# 本物の lam で examples/oneliner/<名前>.out を作る。-i で例を書き換えない
# よう、フォルダーの写しの中で動かす。--check なら、くらべるだけ
set -eu
cd "$(dirname "$0")"
lam=$(cabal list-bin -v0 exe:lam)
check=no
[ "${1:-}" = --check ] && check=yes
status=0
for cmd in examples/oneliner/*.cmd; do
  name=${cmd%.cmd}
  tmp=$(mktemp -d)
  mkdir -p "$tmp/examples"
  cp -R examples/oneliner "$tmp/examples/"
  input=/dev/null
  [ -e "$name.in" ] && input=$PWD/$name.in
  set +e
  (cd "$tmp" && eval "\"$lam\" $(cat "$PWD/$cmd")" < "$input" > out.txt 2> err.txt)
  code=$?
  set -e
  {
    cat "$tmp/out.txt" "$tmp/err.txt"
    [ "$code" = 0 ] || echo "[exit $code]"
    for f in "$tmp"/examples/oneliner/*; do
      case "$f" in *.cmd|*.out|*.in) continue ;; esac
      base=examples/oneliner/$(basename "$f")
      if [ ! -e "$base" ] || ! cmp -s "$f" "$base"; then
        echo "== $base =="
        cat "$f"
      fi
    done
  } > "$tmp/shown.txt"
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
