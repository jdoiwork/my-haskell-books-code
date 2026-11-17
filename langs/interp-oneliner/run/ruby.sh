# Ruby one-liners (Ruby 4.0, on the Mac). Run by check_oneliner.py.
# Ruby のワンライナー（Mac の Ruby 4.0）。check_oneliner.py が動かす

# ANCHOR: book
# Ruby

# ANCHOR: grep
# the lines with ERROR
# ERROR のある行
ruby -ne 'print if $_ =~ /ERROR/' app.log
# ANCHOR_END: grep

# ANCHOR: sed
# colour into color (gsub changes $_)
# gsub は $_ を書き換える
ruby -pe 'gsub(/colour/, "color")' notes.txt
# ANCHOR_END: sed

# ANCHOR: column
# the first column
# 1列目
ruby -F, -lane 'puts $F[0]' sales.csv
# ANCHOR_END: column

# ANCHOR: total
# count times price, added up
# 個数×値段の合計
ruby -F, -lane 'BEGIN { $t = 0 }; $t += $F[1].to_i * $F[2].to_i
  END { puts "total #{$t}" }' sales.csv
# ANCHOR_END: total

# ANCHOR: inplace
# change the file, keep the old one
# その場で書き換え
ruby -i.bak -pe 'gsub(/colour/, "color")' notes.txt
# ANCHOR_END: inplace
# ANCHOR_END: book
