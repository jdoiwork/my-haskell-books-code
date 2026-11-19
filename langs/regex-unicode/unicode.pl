# The Perl code in the Unicode chapter of MiniRegex: \d is Unicode, /a
# for ASCII, \p{Han}, and /i folds ß into ss, as PCRE2 does not.
# MiniRegex の Unicode の章の Perl のコード：\d は Unicode、ASCII には /a、
# \p{Han}、/i は ß を ss に畳む（PCRE2 は畳まない）
use strict;
use warnings;
use utf8;
binmode STDOUT, ":encoding(UTF-8)";

# ANCHOR: digits
# Perl: \d is every digit; with /a, only 0 to 9
# Perl：\d は全部の数字。/a なら 0 から 9 だけ
my $record = "２０２６年１１月１９日、2026-11-19";
my @all   = $record =~ /(\d{4})/g;
my @ascii = $record =~ /(\d{4})/ga;
print "@all | @ascii\n";
# ANCHOR_END: digits

# ANCHOR: scripts
# Perl: \p{Katakana} is the script extensions (ー too), as PCRE2
# Perl：\p{Katakana} は Script_Extensions（ー も入る）。PCRE2 と同じ
"記録室のノート" =~ /(\p{Han}+).*?(\p{Katakana}+)/ and print "$1 $2\n";
# ANCHOR_END: scripts

# ANCHOR: caseless
# Perl: /i folds fully: ß is ss (PCRE2 and MiniRegex: no)
# Perl：/i は完全に畳む：ß は ss（PCRE2 と MiniRegex は「ちがう」）
for my $text ("ẞ", "SS", "\x{212a}") {
    my $pat = $text eq "\x{212a}" ? "k" : "ß";
    print $text =~ /^$pat$/i ? "yes " : "no ";
}
print "\n";
# ANCHOR_END: caseless
