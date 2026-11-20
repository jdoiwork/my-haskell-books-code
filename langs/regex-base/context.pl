# The Perl code in the regex-base chapter of MiniRegex: the same =~
# gives different answers in different places (contexts).
# check_tilde.py runs it with Perl 5.44 in Docker.
# MiniRegex の regex-base の章の Perl のコード：同じ =~ が、置き場所
# （文脈）で別の答えを返す。check_tilde.py が Docker の Perl 5.44 で動かす
use strict;
use warnings;

my $line = "2026-11-21 Mira sold 300 gold, Ruby sold 25 gold";

# ANCHOR: context
# Perl: one =~, three places
# Perl：1つの =~ を、3つの場所に
if ($line =~ /(\w+) sold (\d+)/) {
    print "found: $1 $2\n";             # in if: true or false
}
my @first = $line =~ /(\w+) sold (\d+)/;   # in a list: the groups
print "groups: @first\n";
my @all = $line =~ /(\d+) gold/g;          # with g: every match
print "all: @all\n";
my $count = () = $line =~ /gold/g;         # the count, by a trick
print "count: $count\n";
# ANCHOR_END: context
