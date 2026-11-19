# pcre2test で確かめた答え

ST の VM の章（`book/regex-vm-st.md`）が見せる、本物の PCRE2 の答えです。
`*.txt` を `pcre2test` に読ませると、1行目の版の行のあとに、`*.out` と同じものが出ます（PCRE2 10.49 で確かめた）。

```sh
pcre2test frames.txt | tail -n +2 | diff - frames.out
```

`frames.txt` は、修飾子 `heapframes_size` で、`pcre2_match()` がバックトラックのフレームに使ったヒープの大きさを聞きます。
入力の `\[ab]{5000}` は、`ab` を 5000 回くり返したもの（pcre2test の書き方）です。
