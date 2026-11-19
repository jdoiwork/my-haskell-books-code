# The Perl code in the parser chapter of MiniRegex: how Perl tells a
# wrong pattern ("<-- HERE"), and that a{5,2} is only a warning.
# check_perl.py runs it in Docker.
# MiniRegex のパーサーの章の Perl のコード：Perl が、まちがったパターンを
# どう知らせるか（「<-- HERE」）。a{5,2} は警告だけ。
# check_perl.py が Docker で動かす
use strict;
use warnings;

# ANCHOR: errors
# Perl: the message marks the place with <-- HERE
# Perl：文の中の <-- HERE が、場所の印
for my $p ('[z-a]', '(ab', 'ab)', 'a**') {
    eval { qr/$p/ };
    my ($why) = split / at /, $@;
    print "$why\n";
}
# ANCHOR_END: errors

# ANCHOR: order
# Perl: a{5,2} is a pattern that never matches, with a warning
# Perl：a{5,2} は、決して合わないパターン。警告が出るだけ
local $SIG{__WARN__} = sub { my ($w) = split / at /, $_[0]; print "$w\n" };
my $p = 'a{5,2}';
my $never = qr/$p/;
print "aaaaa" =~ $never ? "match\n" : "no match\n";
# ANCHOR_END: order
