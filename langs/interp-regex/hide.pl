# The Perl code in the chapter on regular expressions of Lam: s/// and
# =~, the short ways Lam's m//, s/// and =~ took after.
# check_perl.py runs it in Docker.
# Lam の正規表現の章の Perl のコード：Lam の m//・s///・=~ のお手本の、
# s/// と =~。check_perl.py が Docker で動かす
use strict;
use warnings;

my $text = "call 080-9876-5432 or 03-1234-5678 now";

# ANCHOR: hide
# Perl: s/// rewrites the variable; /r gives a new string instead
# Perl：s/// は変数を書き換える。/r を付けると、新しい文字列を返す
my $hidden = $text =~ s/\d{2,4}-\d{2,4}-(?<last>\d{4})/***-****-$+{last}/gr;
print "$hidden\n";
# ANCHOR_END: hide

# ANCHOR: match
# Perl: =~ in an if, and the named groups in %+
# Perl：if の中の =~。名前付きのグループは %+ に入る
my $line = "2026-11-20 09:15:40 ERROR order 1043";
if ($line =~ m/^(?<date>\S+) \S+ (?<level>[A-Z]+)/) {
    print "$+{date} $+{level}\n";
}
# ANCHOR_END: match

# ANCHOR: typo
# Perl: a group name written wrong is found only as it runs (a warning)
# Perl：グループの名前の書きまちがいは、動いたときに警告で知るだけ
my $typo = $text =~ s/\d{2,4}-\d{2,4}-(?<last>\d{4})/***-****-$+{lsat}/gr;
print "$typo\n";
# ANCHOR_END: typo
