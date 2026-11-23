# The same recipes with the classic tools of the shell (grep, sort, uniq,
# tr and the awk of macOS). Run by check_recipes.py.
# 同じレシピを、シェルの昔からの道具で（grep・sort・uniq・tr と macOS の
# awk）。check_recipes.py が動かす

# ANCHOR: text-config
# sh: no comments, no blank lines
# コメントと空行を除く
grep -v '^#' settings.conf | grep -v '^[[:space:]]*$'
# ANCHOR_END: text-config

# ANCHOR: text-unique
# awk: a line is printed the first time it is seen
# 初めて見た行だけを出す
awk '!seen[$0]++' fruits.txt
# ANCHOR_END: text-unique

# ANCHOR: text-words
# sh: small letters, one word in a line, count, the 5 most
# 小文字に、1行に1語に、数えて、多い5つ
tr 'A-Z' 'a-z' < words.txt | tr -cs 'a-z' '\n' | grep . \
  | sort | uniq -c | sort -k1,1nr -k2 | head -5
# ANCHOR_END: text-words

# ANCHOR: text-status
# awk: count the column 5 when it is 3 digits
# 5列目が3けたなら数える
awk '$5 ~ /^[0-9][0-9][0-9]$/ { n[$5]++ }
     END { for (s in n) print s, n[s] }' app.log | sort
# ANCHOR_END: text-status

# ANCHOR: git-authors
# sh: the authors, counted, the most first
# 作者を数えて、多い順に
git -C shelf log --all --no-merges --format=%an \
  | sort | uniq -c | sort -k1,1nr -k2
# ANCHOR_END: git-authors
