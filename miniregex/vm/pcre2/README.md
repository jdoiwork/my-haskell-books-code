# pcre2test で確かめた答え

命令の列の章（`book/regex-vm.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test limit.txt | tail -n +2 | diff - limit.out
```

`*.txt` は UTF-8 で書いてあります。修飾子 `utf` のない問題は、PCRE2 がバイトの並びとして読みます。
