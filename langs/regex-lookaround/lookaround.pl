# The Perl code in the chapter on looking around of MiniRegex: a
# lookbehind up to 255 characters, \K (born in Perl), and commas.
# MiniRegex のまわりを見る章の Perl のコード：255 文字までの後読み、
# \K（Perl 生まれ）、カンマ
use strict;
use warnings;
use utf8;
no warnings 'experimental::vlb';
binmode STDOUT, ":encoding(UTF-8)";

my $ledger = "price:1200 gold, price: 80 gold, price:   7 gold";

# ANCHOR: behind
# Perl: a lookbehind may change its length, up to 255 characters;
#   (a|aa) tries the farther one first, as PCRE2 does
# Perl：後読みは長さが変わってもよい。255 文字まで。(a|aa) は、
#   PCRE2 と同じく、遠いほうから試す
my @prices = $ledger =~ /(?<=price:\s?)\d+/g;
print "@prices\n";
for my $p ('(?<=price:\s*)\d+') {
    eval { qr/$p/ };
    print "$p -> ", $@ =~ /(Lookbehind longer than 255)/, "\n";
}
"aab" =~ /(?<=(a|aa))b/ and print "$1\n";
# ANCHOR_END: behind

# ANCHOR: keep
# Perl: \K keeps what is after it; no limit to what is before it
# Perl：\K は、うしろだけを残す。手前の長さに上限はない
my @kept = $ledger =~ /price:\s*\K\d+/g;
print "@kept\n";
(my $money = "1500000") =~ s/\d\K(?=(?:\d{3})+\b)/,/g;
print "$money\n";
# ANCHOR_END: keep
