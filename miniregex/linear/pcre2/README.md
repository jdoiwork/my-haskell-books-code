# pcre2test で確かめた答え

線形時間の章（`book/regex-linear.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test linear.txt | tail -n +2 | diff - linear.out
```

`linear.txt` は、名前の欄でふつうのマッチャーがあきらめ（error -47）、DFA のマッチャー（`\=dfa`）が答えること、`a|ab` の左の枝が先の決まりと、DFA のマッチャーが全部の一致を長い順に返すこと、くり返しの空の回もグループを書くこと、DFA のマッチャーに後方参照がないこと（error -42）を確かめます。
