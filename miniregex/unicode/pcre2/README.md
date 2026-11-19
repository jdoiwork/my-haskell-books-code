# pcre2test で確かめた答え

文字クラスと Unicode の章（`book/regex-unicode.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test classes.txt | tail -n +2 | diff - classes.out
```

`*.txt` は UTF-8 で書いてあります。修飾子 `utf` のない問題（`caseless.txt` の1問目）は、PCRE2 がバイトの並びとして読みます。
