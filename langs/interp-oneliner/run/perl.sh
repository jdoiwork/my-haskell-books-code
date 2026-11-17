# Perl one-liners. check_oneliner.py runs each block in a copy of the
# files of code/lam/oneliner/examples/oneliner (Perl 5.44, in Docker).
# Perl のワンライナー。check_oneliner.py が、ブロックごとに、例のファイルの
# 写しの中で動かす（Docker の中の Perl 5.44）

# ANCHOR: book
# Perl

# ANCHOR: grep
# the lines with ERROR
# ERROR のある行
perl -ne 'print if /ERROR/' app.log
# ANCHOR_END: grep

# ANCHOR: sed
# colour into color
# colour を color に
perl -pe 's/colour/color/g' notes.txt
# ANCHOR_END: sed

# ANCHOR: column
# the first column
# 1列目
perl -F, -lane 'print $F[0]' sales.csv
# ANCHOR_END: column

# ANCHOR: count
# count the lines ($. is the number of the line)
# 行を数える
perl -lne 'END { print $. }' app.log
# ANCHOR_END: count

# ANCHOR: total
# count times price, added up
# 個数×値段の合計
perl -F, -lane '$t += $F[1] * $F[2]; END { print "total $t" }' sales.csv
# ANCHOR_END: total

# ANCHOR: average
# the average, with 1 digit after the point
# 平均（小数点以下1けた）
perl -lane '$s += $F[1]; $n++;
  END { printf "average %.1f\n", $s / $n }' scores.txt
# ANCHOR_END: average

# ANCHOR: unique
# each line once, sorted
# 重ならないように、並べて
perl -lne '$seen{$_} = 1; END { print for sort keys %seen }' fruits.txt
# ANCHOR_END: unique

# ANCHOR: inplace
# change the file, keep the old one as notes.txt.bak
# その場で書き換え、元は .bak に
perl -i.bak -pe 's/colour/color/g' notes.txt
# ANCHOR_END: inplace

# ANCHOR: deparse
# show the loop that -n wraps around the program (B::Deparse)
# -n がプログラムのまわりに作るループを見せる（B::Deparse）
perl -MO=Deparse -ne 'print if /ERROR/'
# ANCHOR_END: deparse
# ANCHOR_END: book
