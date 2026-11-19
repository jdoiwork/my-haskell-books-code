# The Perl code in the chapter on recursion of MiniRegex: (?R), (?1),
# (?&name) and (?(DEFINE) ...) came to Perl in 5.10. A call gives the
# groups back as they were, and a recursion going round dies when it
# runs.
# MiniRegex の再帰の章の Perl のコード：(?R)・(?1)・(?&name)・
# (?(DEFINE) ...) は Perl 5.10 から。呼び出しはグループを元に戻し、
# 回り続ける再帰は、動かしたときに止まる
use strict;
use warnings;

my $memo = "Mira (healer (2nd class) from Lam (east)) paid 120 gold (cash)";

# ANCHOR: recurse
# Perl: (?R) calls the whole pattern; (?(DEFINE) ...) names the parts
# Perl：(?R) はパターン全体を呼ぶ。(?(DEFINE) ...) で部品に名前を付ける
my @notes = $memo =~ /\((?:[^()]++|(?R))*\)/g;
print join(" | ", @notes), "\n";
"2026-11-01..2026-11-19" =~ /
  (?(DEFINE) (?<date> \d{4}-\d{2}-\d{2} ))
  (?<from> (?&date)) \.\. (?<to> (?&date))
/x and print "$+{from} $+{to}\n";
# ANCHOR_END: recurse

# ANCHOR: keeps
# Perl: a call gives the group back as it was; going round dies
# Perl：呼び出しはグループを元に戻す。回り続けると、止まる
"sense and responsibility" =~ /(sens|respons)e and (?1)ibility/
  and print "$1\n";
eval { "xb" =~ /a|(?R)b/ };
print $@ =~ /^(Infinite recursion in regex)/, "\n";
# ANCHOR_END: keeps
