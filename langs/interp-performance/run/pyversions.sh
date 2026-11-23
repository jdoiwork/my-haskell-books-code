#!/bin/sh
# Run pyversions.py on each version of Python that uv put in (the same
# way of building for each), and on the Homebrew and Apple ones.
#   sh pyversions.sh LOG
# uv で入れた Python の版（どれも同じ作り方）と、Homebrew・Apple の
# Python で、pyversions.py を動かす
set -eu
here=$(cd "$(dirname "$0")" && pwd)
for v in cpython-3.8.20 cpython-3.9.25 cpython-3.10.20 cpython-3.11.15 \
         cpython-3.12.13 cpython-3.13.14 cpython-3.14.6 pypy-3.11.15; do
  "$(uv python find --no-project "$v")" "$here/pyversions.py" "$1"
done
/opt/homebrew/bin/python3.15 "$here/pyversions.py" "$1"
/usr/bin/python3 "$here/pyversions.py" "$1"
