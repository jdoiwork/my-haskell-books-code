# The Bash code in the regex-base chapter: =~ inside [[ ]] answers with
# the exit status, and leaves the groups in BASH_REMATCH.
# check_tilde.py runs it with Bash 5.3 in Docker.
# regex-base の章の Bash のコード：[[ ]] の中の =~ は、終了ステータスで
# 答え、グループを BASH_REMATCH に残す。check_tilde.py が Docker の
# Bash 5.3 で動かす
line="2026-11-21 Mira sold 300 gold, Ruby sold 25 gold"

# ANCHOR: bash
# Bash: the answer is the exit status (0 is "found")
# Bash：答えは終了ステータス（0 が「見つかった」）
if [[ $line =~ ([A-Za-z]+)\ sold\ ([0-9]+) ]]; then
  echo "status: $?"
  echo "found: ${BASH_REMATCH[1]} ${BASH_REMATCH[2]}"
fi
[[ $line =~ dragon ]]
echo "no dragon: $?"
# ANCHOR_END: bash
