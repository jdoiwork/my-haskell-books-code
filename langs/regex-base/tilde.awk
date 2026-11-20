# The awk code in the regex-base chapter: ~ is 1 or 0. check_tilde.py
# runs it with the awk of macOS.
# regex-base の章の awk のコード：~ は 1 か 0。check_tilde.py が
# macOS の awk で動かす

# ANCHOR: awk
# awk: ~ is a number, 1 (found) or 0
# awk：~ は数で、1（見つかった）か 0
{
  print "gold:", ($0 ~ /[0-9]+ gold/), "dragon:", ($0 ~ /dragon/)
  if (match($0, /[0-9]+ gold/)) print "at:", RSTART, RLENGTH
}
# ANCHOR_END: awk
