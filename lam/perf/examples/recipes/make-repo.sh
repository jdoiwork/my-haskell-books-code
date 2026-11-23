#!/bin/sh
# Make a small git repository in the folder shelf/, the same every time:
# the same names, the same times, so the same commit hashes. Four
# branches besides main: three merged into it, one not. (The repository
# of the chapter "Like a shell", with TODO lines in the files.)
# フォルダー shelf/ に、小さな git のリポジトリを作る。毎回同じ（同じ名前、
# 同じ時刻なので、コミットのハッシュも同じ）。main のほかにブランチが4つ。
# 3つは main に取りこみ済み、1つはまだ（「シェルのように」の章の
# リポジトリに、TODO の行を足したもの）
set -eu
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
mkdir shelf && cd shelf
# 🖋️ Commit as someone, at a time:  as Python 2026-11-10T10:00:00+09:00 msg
# だれかとして、ある時刻にコミットする
as() {
  who=$1; when=$2; shift 2
  GIT_AUTHOR_NAME="$who" GIT_AUTHOR_EMAIL="$who@example.com" \
  GIT_COMMITTER_NAME="$who" GIT_COMMITTER_EMAIL="$who@example.com" \
  GIT_AUTHOR_DATE="$when" GIT_COMMITTER_DATE="$when" \
    git -c commit.gpgsign=false "$@"
}
commit() { who=$1; when=$2; shift 2; as "$who" "$when" commit -q -m "$*"; }
merge() { who=$1; when=$2; branch=$3; as "$who" "$when" merge -q --no-ff -m "merge $branch" "$branch"; }

git init -q -b main
mkdir -p src
printf 'main = putStrLn "hello"\n' > src/main.lam
git add src
commit Lam 2026-11-01T09:00:00+09:00 start

git switch -q -c fix-typo
printf '%s\n' '-- TODO: say the name' 'main = putStrLn "Hello"' > src/main.lam
git add src && commit Python 2026-11-20T10:00:00+09:00 fix a typo
git switch -q main && merge Lam 2026-11-20T11:00:00+09:00 fix-typo

git switch -q -c add-shell
printf '%s\n' 'run = Shell.ok $"git status"' '-- FIXME: stop when git is not there' > src/run.lam
git add src && commit Rust 2026-11-24T15:00:00+09:00 add a shell command
git switch -q main && merge Lam 2026-11-24T16:00:00+09:00 add-shell

git switch -q -c add-table
printf 'rows = []\n' > src/table.lam
git add src && commit Ruby 2026-11-26T13:00:00+09:00 add a table
printf '%s\n' 'rows = []' '-- TODO: sort the rows by date' > src/table.lam
git add src && commit Ruby 2026-11-26T13:30:00+09:00 note the order
git switch -q main && merge Lam 2026-11-26T14:00:00+09:00 add-table

git switch -q -c try-colors
printf 'red = "31"\n' > src/colors.lam
git add src && commit TS 2026-11-27T17:00:00+09:00 try colors
git switch -q main
printf '%s\n' '-- TODO: say the name' 'main = putStrLn "Hello, shelf!"' > src/main.lam
git add src && commit Python 2026-11-28T09:00:00+09:00 greet the shelf
