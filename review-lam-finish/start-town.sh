#!/bin/sh
# Start the town of sites (town.lam, port 18741) in the background, and
# wait until it listens. For the .setup files of make-out.py.
# サイトの町（town.lam、ポート 18741）を後ろで始め、聞き始めるまで待つ。
# make-out.py の .setup のため
lam town.lam > /dev/null 2>&1 < /dev/null &
n=0
while ! nc -z 127.0.0.1 18741 2>/dev/null; do
  n=$((n + 1)); [ $n -gt 100 ] && break; sleep 0.1
done
