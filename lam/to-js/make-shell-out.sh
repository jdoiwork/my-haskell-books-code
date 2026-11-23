#!/bin/sh
# Make examples/shell/<name>.out with the real lam and the real git, in
# a copy of the folder where make-repo.sh has made a small repository.
# Each line of <name>.cmd is a command; a line that starts with "@ " is
# run too, but its output is not kept. The standard error goes where
# the screen goes, in the order they were written (the questions of
# Term.confirm come before what follows them). With --check, only
# compare.
# 本物の lam と本物の git で examples/shell/<名前>.out を作る。フォルダーの
# 写しの中で、make-repo.sh が小さなリポジトリを作ってから動かす。
# <名前>.cmd の1行が1つのコマンド。「@ 」で始まる行も動かすが、出力は
# 残さない。標準エラーは画面と同じところへ、書いた順に（Term.confirm の
# 問いは、その後ろのものより前に出る）。--check なら、くらべるだけ
set -eu
cd "$(dirname "$0")"
LAMBIN=$(cabal list-bin -v0 exe:lam)
export LAMBIN
# git reads no config of this machine, and colors are left to lam.
# git はこのマシンの設定を読まない。色は lam に任せる
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
unset NO_COLOR
check=no
[ "${1:-}" = --check ] && check=yes
status=0
for cmd in examples/shell/*.cmd; do
  name=${cmd%.cmd}
  tmp=$(mktemp -d)
  cp examples/shell/*.lam examples/shell/make-repo.sh "$tmp/"
  (cd "$tmp" && sh make-repo.sh)
  : > "$tmp/shown.txt"
  while IFS= read -r line; do
    keep=yes
    case "$line" in "@ "*) keep=no; line=${line#@ } ;; esac
    set +e
    (cd "$tmp" && lam() { "$LAMBIN" "$@"; } && eval "$line" > out.txt 2>&1)
    code=$?
    set -e
    if [ "$keep" = yes ]; then
      # The seconds of --trace change from run to run: keep them as 0.
      # --trace の秒数は毎回ちがうので、0 にそろえて残す
      sed 's/^trace [0-9]*\.[0-9]*s:/trace 0.000s:/' "$tmp/out.txt" \
        >> "$tmp/shown.txt"
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
