#!/bin/sh
# Make examples/toolbox/<name>.out with the real lam and the real git,
# in a copy of the folder where make-repo.sh (of the shell chapter) has
# made a small repository. Each line of <name>.cmd is a command; a line
# that starts with "@ " is run too, but its output is not kept. The
# standard error goes where the screen goes. With --check, only compare.
# 本物の lam と本物の git で examples/toolbox/<名前>.out を作る。
# フォルダーの写しの中で、（シェルの章の）make-repo.sh が小さな
# リポジトリを作ってから動かす。<名前>.cmd の1行が1つのコマンド。
# 「@ 」で始まる行も動かすが、出力は残さない。標準エラーは画面と同じ
# ところへ。--check なら、くらべるだけ
set -eu
cd "$(dirname "$0")"
here=$(pwd)
LAMBIN=$(cabal list-bin -v0 exe:lam)
export LAMBIN
# git reads no config of this machine.
# git はこのマシンの設定を読まない
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
check=no
[ "${1:-}" = --check ] && check=yes
status=0
for cmd in examples/toolbox/*.cmd; do
  name=${cmd%.cmd}
  tmp=$(mktemp -d)
  cp -R examples/toolbox/. "$tmp/"
  rm -f "$tmp"/*.cmd "$tmp"/*.out
  (cd "$tmp" && sh "$here/examples/shell/make-repo.sh")
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
