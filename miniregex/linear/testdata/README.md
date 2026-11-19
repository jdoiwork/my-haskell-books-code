# PCRE2 のテストデータ

`testoutput1` は、PCRE2 10.49（2026-09-28）の配布物の `testdata/testoutput1` を、そのまま写したものです。
取ってきた場所：<https://github.com/PCRE2Project/pcre2/blob/pcre2-10.49/testdata/testoutput1>

PCRE2 の `LICENCE.md` によると、`testdata` のフォルダーのデータは著作権がなく、パブリックドメインです
（"The data in the testdata directory is not copyrighted and is in the public domain."）。

PCRE2 の試験の道具 `pcre2test` に `testinput1` を読ませたときの出力で、パターン・入力・期待する答えが、
全部この1つのファイルに入っています。互換度メーター（`cabal run compat-meter`）が読みます。

ファイルは UTF-8 ではなく、1バイトが1文字です（`0x81` のような 128 以上のバイトが、そのまま入っています）。
はじめは、取ってくるときに UTF-8 に化けていた（`0x81` が `0xC2 0x81` の2バイトになっていた）のを、2026-11-19 に元のバイトに戻しました。

## testoutput4（2冊目、2026-11-19 に足した）

`testoutput4` は、PCRE2 10.49 の配布物の `testdata/testoutput4` を、そのまま写したものです（同じくパブリックドメイン）。
取ってきた場所：<https://github.com/PCRE2Project/pcre2/blob/pcre2-10.49/testdata/testoutput4>

`testinput4` の出力で、UTF と Unicode の性質（`\p{…}` や大文字と小文字）を確かめる、Perl と同じに動く機能のテストです。
こちらは UTF-8 のファイルです。Homebrew の `pcre2test`（10.49）に `testinput4` を読ませた出力が、このファイルと1文字も違わないことを確かめました。
文字クラスと Unicode の章（`book/regex-unicode.md`）から、メーターが2冊目として読みます。
