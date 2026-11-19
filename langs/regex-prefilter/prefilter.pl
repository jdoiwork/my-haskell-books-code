# Perl looks at a pattern before it searches, too: re::regmust tells
# the plain text every match needs (anchored: at a fixed place from the
# start; floating: somewhere).
# Perl も、探す前にパターンを見る。re::regmust は、どの一致にも要る
# ただの文字列を教える（anchored：頭から決まった場所に、floating：
# どこかに）
use strict;
use warnings;
use re qw(regmust);

# ANCHOR: regmust
# Perl: the text each pattern surely needs
# Perl：パターンごとに、必ず要る文字列
for my $p (qr/Room (\d+)/, qr/(a+)*b/, qr/(\w+)@(\w+)\.com/,
           qr/\b\d{4}-\d{2}-\d{2}\b/, qr/colou?r|grey/) {
    my ($anchored, $floating) = regmust($p);
    printf "%-26s anchored %-8s floating %s\n", $p,
        map { length $_ ? "\"$_\"" : "-" } $anchored, $floating;
}
# ANCHOR_END: regmust
