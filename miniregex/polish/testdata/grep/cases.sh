# The cases of minigrep, run with $GREP: the same lines come out of
# minigrep, GNU grep -P, ripgrep and the grep of macOS (-E). Run in this
# folder; what each prints is compared with expected.out.
# minigrep のためしの一覧。$GREP で動かす。minigrep・GNU grep -P・ripgrep・
# macOS の grep（-E）から同じ行が出る。このフォルダーで動かし、出たものを
# expected.out とくらべる
run() {
  echo "== $*"
  $GREP "$@" 2>/dev/null
  echo "exit $?"
}
run gold guild.log
run -c gold guild.log
run -ic gold guild.log
run -v gold guild.log
run -on '\d+ gold' guild.log
run -o '\w+ sold' guild.log
run -c gold guild.log notes.log
run silver guild.log
run gold nofile notes.log
run '(\d+' guild.log
