#!/bin/sh
# Make release/<name>.out with the real lam (of lam/oneliner) and the
# real git, in a copy of release/ where make-repo.sh has made the small
# repository of the workshop. Each line of <name>.cmd is a command. The
# release commit is made by Lam, at a fixed time, so the hashes and the
# archive are the same every time. With --check, only compare.
# 本物の lam（lam/oneliner のもの）と本物の git で release/<名前>.out を作る。
# release/ の写しの中で、make-repo.sh が工房の小さなリポジトリを作ってから
# 動かす。<名前>.cmd の1行が1つのコマンド。リリースのコミットは Lam が
# 決まった時刻に作るので、ハッシュも固めたファイルも毎回同じ。
# --check なら、くらべるだけ
set -eu
cd "$(dirname "$0")"
LAMBIN=$(cd ../lam/oneliner && cabal list-bin -v0 exe:lam)
export LAMBIN
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
export GIT_AUTHOR_NAME=Lam GIT_AUTHOR_EMAIL=Lam@example.com
export GIT_COMMITTER_NAME=Lam GIT_COMMITTER_EMAIL=Lam@example.com
export GIT_AUTHOR_DATE=2026-11-30T10:00:00+09:00
export GIT_COMMITTER_DATE=2026-11-30T10:00:00+09:00
unset NO_COLOR
check=no
[ "${1:-}" = --check ] && check=yes
status=0
for cmd in release/*.cmd; do
  name=${cmd%.cmd}
  tmp=$(mktemp -d)
  cp release/*.lam release/*.txt release/make-repo.sh "$tmp/"
  (cd "$tmp" && sh make-repo.sh)
  : > "$tmp/shown.txt"
  while IFS= read -r line; do
    set +e
    (cd "$tmp" && lam() { "$LAMBIN" "$@"; } && eval "$line" > out.txt 2>&1)
    code=$?
    set -e
    cat "$tmp/out.txt" >> "$tmp/shown.txt"
    [ "$code" = 0 ] || echo "[exit $code]" >> "$tmp/shown.txt"
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
