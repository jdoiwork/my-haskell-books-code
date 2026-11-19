# The Perl code in the chapter on the VM of MiniRegex: the program Perl
# compiles a pattern into (use re 'debug'), and a search that Perl's
# cache keeps short.
# MiniRegex の命令の列の章の Perl のコード：Perl がパターンから作る
# プログラム（use re 'debug'）と、Perl のキャッシュが短くする探しもの
use strict;
use warnings;
use Time::HiRes qw(time);

# (the debug output goes to STDERR, when the program is compiled; send
#  it where the rest goes, before that)
# （debug の出力は、プログラムをコンパイルするときに STDERR に出る。
#   その前に、ほかと同じところへ送っておく）
BEGIN { open STDERR, '>&', \*STDOUT or die; $| = 1; }

{
  # ANCHOR: program
  # Perl: use re 'debug' prints the program of a pattern
  # Perl：use re 'debug' で、パターンのプログラムを見せてくれる
  use re 'debug';
  my $colour = qr/colou?r|gr(a|e)y/;
  # ANCHOR_END: program
}

# ANCHOR: cache
# Perl: the same search as Python's and TypeScript's, at once
# Perl：Python・TypeScript と同じ探しものが、すぐ終わる
my $field = "Mira Ruby Lam GHC Perl Mira Ruby Lam GHC!";
my $start = time;
my $found = $field =~ /^(\w+\s?)+$/;
printf "%s, %.2fs\n", ($found ? "found" : "not found"), time - $start;
# ANCHOR_END: cache
