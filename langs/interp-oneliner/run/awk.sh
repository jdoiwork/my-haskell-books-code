# awk one-liners (the awk of macOS). Run by check_oneliner.py.
# awk のワンライナー（macOS の awk）。check_oneliner.py が動かす

# ANCHOR: book
# awk

# ANCHOR: grep
# the lines with ERROR
# ERROR のある行
awk '/ERROR/' app.log
# ANCHOR_END: grep

# ANCHOR: column
# the first column
# 1列目
awk -F, '{ print $1 }' sales.csv
# ANCHOR_END: column

# ANCHOR: count
# count the lines (NR)
# 行を数える
awk 'END { print NR }' app.log
# ANCHOR_END: count

# ANCHOR: total
# count times price, added up
# 個数×値段の合計
awk -F, '{ t += $2 * $3 } END { print "total", t }' sales.csv
# ANCHOR_END: total

# ANCHOR: average
# the average
# 平均
awk '{ s += $2; n++ } END { printf "average %.1f\n", s / n }' scores.txt
# ANCHOR_END: average

# ANCHOR: unique
# each line once, in the order they came
# 最初に出てきた順に1回ずつ
awk '!seen[$0]++' fruits.txt
# ANCHOR_END: unique
# ANCHOR_END: book
