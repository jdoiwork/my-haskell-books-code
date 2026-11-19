# The Perl code in the groups chapter of MiniRegex: $1, $+{name} and the
# places @- and @+, and a group in a repeat.
# MiniRegex のグループの章の Perl のコード：$1・$+{name}・場所の @- と @+、
# くり返しの中のグループ
use strict;
use warnings;

# ANCHOR: groups
# Perl: $1 is group 1, $+{day} the group named day, and $-[2] and
#   $+[2] where group 2 starts and ends
# Perl：$1 は1番のグループ、$+{day} は day という名前のグループ。
#   $-[2] と $+[2] は、2番のグループの始まりと終わり
if ("On 2026-11-19" =~ /(?<y>\d{4})-(?<m>\d\d)-(?<day>\d\d)/) {
    print "$+{day}/$2/$1 $-[2]..$+[2]\n";
}
# ANCHOR_END: groups

# ANCHOR: repeat
# Perl: a group in a repeat forgets, at the start of each round
# Perl：くり返しの中のグループは、回の頭で忘れる
"ab" =~ /(?:(a)|b)*/ and print defined $1 ? "$1\n" : "undef\n";
# ANCHOR_END: repeat

# ANCHOR: findall
# Perl: /g in a list gives all the matches, as PCRE2 does
# Perl：リストの中の /g で、全部の一致。PCRE2 と同じ
my @all = ("a" =~ /a??/g);
print join(",", map { "<$_>" } @all), "\n";
# ANCHOR_END: findall
