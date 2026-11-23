#!/bin/sh
# Make the folder downloads/ of the recipes on files: the same names and
# sizes every time, and old times (from now) for some of them.
# ファイルのレシピの downloads/ を作る。名前と大きさは毎回同じ。いくつかは
# 古い時刻に（いまから数えて）
set -eu
# 📦 A file of n kilobytes of the letter c.
# 文字 c が n キロバイトのファイル
fill() { head -c $(( $2 * 1024 )) /dev/zero | tr '\0' "$3" > "$1"; }
mkdir -p downloads/logs
fill downloads/movie.mp4 5120 m
fill downloads/archive.zip 2048 z
fill "downloads/My Photo 01.JPG" 900 p
fill "downloads/my photo 02.jpg" 700 q
fill downloads/report.pdf 300 r
printf 'buy milk\ncall Rust\n' > downloads/notes.txt
cp downloads/notes.txt "downloads/notes (1).txt"
printf 'date,amount\n2026-11-01,1200\n' > downloads/data.csv
cp downloads/data.csv downloads/logs/data-copy.csv
# 🕰️ Two logs written 60 and 40 days ago (BSD date, as on macOS).
# 60 日前と 40 日前に書いたログ（macOS の BSD の date で）
printf 'sixty\n' > downloads/logs/sixty-days.log
printf 'forty\n' > downloads/logs/forty-days.log
printf 'today\n' > downloads/logs/today.log
touch -t "$(date -v-60d +%Y%m%d%H%M)" downloads/logs/sixty-days.log
touch -t "$(date -v-40d +%Y%m%d%H%M)" downloads/logs/forty-days.log
