# The Perl code in the matcher chapter of MiniRegex: which branch wins.
# MiniRegex のマッチャーの章の Perl のコード：どちらの枝が勝つか
use strict;
use warnings;

# ANCHOR: first
# Perl: $& is what was found; the left branch wins
# Perl：$& が見つけたもの。左の枝が勝つ
"abc" =~ /a|ab/ and print "$&\n";
"aaab" =~ /a*?b/ and print "$&\n";
# ANCHOR_END: first
