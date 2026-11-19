# The Perl code in the chapter on the cursor and the instructions: the
# final programs Perl compiles for four repeats. A repeat of a simple
# thing (one character) is STAR or PLUS; a body of a fixed length is
# CURLYM; anything else is CURLYX ... WHILEM.
# しおりと命令を磨く章の Perl のコード：4つのくり返しに、Perl が
# コンパイルする最後のプログラム。単純なもの（1文字）のくり返しは
# STAR か PLUS、長さの決まった中身は CURLYM、それ以外は CURLYX … WHILEM
# ANCHOR: repeat
use re qw(Debug COMPILE);

# Perl: four repeats (the program goes to standard error)
# Perl：4つのくり返し（プログラムは標準エラーに出る）
qr/a*b/;
qr/(?:ab)*c/;
qr/^(\w+\s?)+$/;
qr/(?:a|)*b/;
# ANCHOR_END: repeat
