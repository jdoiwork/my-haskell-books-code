# The Perl code in the design chapter of MiniRegex: =~ with the g flag,
# in list context, gives every date. check_perl.py runs it in Docker.
# MiniRegex の設計の章の Perl のコード：g のフラグを付けた =~ を、
# リストの文脈で使うと、日付が全部返る。check_perl.py が Docker で動かす
use strict;
use warnings;

my $records = <<'END';
2026-11-19 Mira sold a dragon scale: 300 gold
2026-11-20 Ruby bought herb, sword, shield
On 2026-11-21 a dragon was seen near the lake
Room 1234-56-789 is closed
END

# ANCHOR: dates
# Perl: every date, in one line
# Perl：日付を全部、1行で
my @dates = $records =~ /[0-9]{4}-[0-9]{2}-[0-9]{2}/g;
print "@dates\n";
# ANCHOR_END: dates
