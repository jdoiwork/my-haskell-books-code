#!/bin/sh
# Make the small repository of the workshop in this folder, the same
# every time (the same names and times, so the same hashes). The last
# release is v0.25.0; six commits came after it.
# このフォルダーに、言語工房の小さなリポジトリを作る。毎回同じ（同じ名前と
# 時刻なので、ハッシュも同じ）。最後のリリースは v0.25.0 で、そのあとに
# コミットが6つ
set -eu
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
# 🖋️ Commit as someone, at a time:  commit Rust 2026-11-25T10:00:00+09:00 msg
# だれかとして、ある時刻にコミットする
commit() {
  who=$1; when=$2; shift 2
  git add -A
  GIT_AUTHOR_NAME="$who" GIT_AUTHOR_EMAIL="$who@example.com" \
  GIT_COMMITTER_NAME="$who" GIT_COMMITTER_EMAIL="$who@example.com" \
  GIT_AUTHOR_DATE="$when" GIT_COMMITTER_DATE="$when" \
    git -c commit.gpgsign=false commit -q -m "$*"
}

git init -q -b main
# The tools (*.lam here) and what they make are not a part of it.
# 道具（ここの *.lam）と、道具が作るものは、リポジトリに入れない
cat > .gitignore <<'EOF'
/*.lam
/make-repo.sh
/*.new
/dist/
EOF
cat > lam.toml <<'EOF'
[package]
name = "lam"
version = "0.25.0"
EOF
cat > CHANGELOG.md <<'EOF'
# Changelog

## 0.25.0 (2026-11-23)

### Features

- **oneliner**: run Lam in one line, like Perl
EOF
mkdir -p src
printf 'main = putStrLn "lam"\n' > src/main.lam
commit Lam 2026-11-23T18:00:00+09:00 \
  "feat(oneliner): run Lam in one line, like Perl"
git tag v0.25.0

printf 'shell = 1\n' > src/shell.lam
commit Rust 2026-11-24T10:00:00+09:00 "feat(shell): pass a value as one word"
printf 'table = 2\n' > src/table.lam
commit Ruby 2026-11-25T11:00:00+09:00 "fix(表): count a kanji as two columns"
printf '# Lam\n' > README.md
commit TS 2026-11-26T15:00:00+09:00 "docs: explain --print-script"
printf 'edit = 3\n' > src/edit.lam
commit Python 2026-11-27T09:00:00+09:00 "fix: keep the file when -i stops"
printf 'tidy = 4\n' > src/tidy.lam
commit Go 2026-11-28T14:00:00+09:00 "chore: tidy the examples"
printf 'trace = 5\n' > src/trace.lam
commit Erlang 2026-11-29T16:00:00+09:00 "feat: show the time with --trace"
