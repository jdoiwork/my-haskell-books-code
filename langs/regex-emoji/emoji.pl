# The Perl code in the emoji chapter of MiniRegex: length counts code
# points, \X counts what a person sees, and \p{Emoji} takes 7 and #.
# MiniRegex の絵文字の章の Perl のコード：length はコードポイントを数え、
# \X は人が見る文字を数える。\p{Emoji} は 7 や # も取る
use strict;
use warnings;
use utf8;
binmode STDOUT, ":encoding(UTF-8)";

# 👨‍👩‍👧: a man, a woman and a girl, with ZWJs between
# 👨‍👩‍👧：男の人、女の人、女の子のあいだに ZWJ
my $family = "\x{1F468}\x{200D}\x{1F469}\x{200D}\x{1F467}";

# ANCHOR: count
# Perl: length is code points; () = ... =~ /\X/g counts the clusters
# Perl：length はコードポイント。() = ... =~ /\X/g でクラスターを数える
my $clusters = () = "$family🇯🇵👍🏽" =~ /\X/g;
print length($family), " ", $clusters, "\n";
my @found = "No.7 #1" =~ /(\p{Emoji})/g;
print "@found\n";
# ANCHOR_END: count
