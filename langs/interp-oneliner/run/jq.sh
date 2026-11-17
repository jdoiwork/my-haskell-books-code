# jq one-liners (jq 1.8.2, the official image ghcr.io/jqlang/jq in
# Docker). Run by check_oneliner.py.
# jq のワンライナー（Docker の中の jq 1.8.2）。check_oneliner.py が動かす

# ANCHOR: book
# jq

# ANCHOR: json_users
# the user of each order ([] opens the array)
# 注文ごとの user（[] で配列を開く）
jq '.orders[].user' shop.json
# ANCHOR_END: json_users

# ANCHOR: json_items
# the items of the first order, one line each (-c)
# 最初の注文の品物を、1行ずつ（-c）
jq -c '.orders[0].items[]' shop.json
# ANCHOR_END: json_items

# ANCHOR: jsonl_errors
# the lines of JSON Lines whose level is error (-S sorts the keys)
# level が error の行（-S でキーを並べる）
jq -cS 'select(.level == "error")' events.jsonl
# ANCHOR_END: jsonl_errors

# ANCHOR: jsonl_names
# the name of the user on each line
# 各行の user の name
jq '.user.name' events.jsonl
# ANCHOR_END: jsonl_names

# ANCHOR: jsonl_sum
# all the lines in one array (-s), then add up ms
# 全部の行を1つの配列にして（-s）、ms を足す
jq -s 'map(.ms) | add' events.jsonl
# ANCHOR_END: jsonl_sum
# ANCHOR_END: book
