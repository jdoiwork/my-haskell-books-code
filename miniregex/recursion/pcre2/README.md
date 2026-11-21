# pcre2test で確かめた答え

再帰の章（`book/regex-recursion.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test recursion.txt | tail -n +2 | diff - recursion.out
```

`recursion.txt` は、`(?R)`・`(?1)`・`(?&name)` の答え、呼び出しがグループを書き換えないこと、回文（再帰の中へ戻る）、同じ場所で回る再帰（error -52）、`++` のない再帰のステップ数（error -47）を確かめます。
