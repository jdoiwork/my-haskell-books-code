# The chapter on learning from other lands (MiniRegex): what Perl knows
# about the guild's jobs before it starts matching. Compiles each
# pattern with `use re qw(Debug COMPILE)`, which tells (on standard
# error) the fixed text it looks for first, and the class a match
# starts with. check_lessons.py runs it with Homebrew's Perl 5.44 and
# keeps only those lines.
# よその国に学ぶ章（MiniRegex）：一致を探し始める前に、Perl がギルドの
# 仕事について知っていること。どのパターンも `use re qw(Debug COMPILE)`
# でコンパイルする。先に探す決まった文字列と、一致の始まりの文字の種類
# を（標準エラーに）教えてくれる。check_lessons.py が Homebrew の
# Perl 5.44 で動かし、その行だけを残す
use strict;
use warnings;
use re qw(Debug COMPILE);

# ANCHOR: intuit
my @jobs = (
    [ dates    => '\d{4}-\d{2}-\d{2}' ],
    [ mail     => '[\w.+-]+@[\w-]+\.[\w.-]+' ],
    [ names    => 'Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go' ],
    [ caseless => '(?i)perl' ],
    [ sales    => '(\w+) sold (\d+) (\w+)' ],
);

for my $job (@jobs) {
    my ($name, $pattern) = @$job;
    print STDERR "== $name\n";
    my $re = qr/$pattern/;
}
# ANCHOR_END: intuit
