# The chapter on measuring (MiniRegex): the same jobs as MiniRegex's
# benchmark, with Perl's own regexes. Run as: perl bench.pl LINES ROUNDS
# (check_benchmarks.py runs it with Homebrew's Perl 5.44).
# Prints one line for each job: the job, the count, the checksum, and
# the middle time of the rounds in microseconds.
# 測って比べる章（MiniRegex）：MiniRegex のベンチマークと同じ仕事を、
# Perl の正規表現で。仕事ごとに1行：仕事、件数、確かめの数、回の
# まん中の時間（マイクロ秒）
use strict;
use warnings;
use Time::HiRes qw(time);

my @NAMES = qw(Mira Ruby Lam GHC Perl Rust Python Go);
my @ITEMS = qw(herbs potions swords maps scrolls);

# 📒 The guild's book of sales: n lines, the same in every language.
# ギルドの売り上げの帳面：n 行。どの言語でも同じもの
sub guild_book {
    my ($n) = @_;
    my $text = '';
    for my $i (0 .. $n - 1) {
        my $seller = $NAMES[$i % 8];
        $text .= sprintf "2026-11-%02d %02d:%02d %s sold %d %s to %s for %d gold",
            $i % 28 + 1, $i % 24, $i * 7 % 60, $seller, $i % 9 + 1,
            $ITEMS[$i % 5], $NAMES[(3 * $i + 1) % 8], $i * 37 % 1000;
        $text .= sprintf " <%s\@guild.example>", lc $seller if $i % 7 == 0;
        $text .= "\n";
    }
    return $text;
}

# ANCHOR: count
# 🔢 Count the matches with //g, from left to right; the checksum adds
#    up group 2 where there is one.
# //g で左から一致を数える。確かめの数は、2番のグループがあれば、
# その数の合計
sub count {
    my ($re, $text) = @_;
    my ($n, $total) = (0, 0);
    while ($text =~ /$re/g) {
        $n++;
        $total += $2 if defined $2;
    }
    return ($n, $total);
}
# ANCHOR_END: count

# 🕰️ The middle time of the rounds, in microseconds.
# 回の、まん中の時間（マイクロ秒）
sub clock {
    my ($rounds, $job) = @_;
    my (@times, @answer);
    for (1 .. $rounds) {
        my $start = time;
        @answer = $job->();
        push @times, (time - $start) * 1e6;
    }
    @times = sort { $a <=> $b } @times;
    return ($times[int($rounds / 2)], @answer);
}

my ($lines, $rounds) = @ARGV;
my $text = guild_book($lines);
my @jobs = (
    [dates    => qr/\d{4}-\d{2}-\d{2}/],
    [mail     => qr/[\w.+-]+@[\w-]+\.[\w.-]+/],
    [names    => qr/Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go/],
    [caseless => qr/(?i)perl/],
    [sales    => qr/(\w+) sold (\d+) (\w+)/],
);
for my $job (@jobs) {
    my ($name, $re) = @$job;
    my ($us, $n, $total) = clock($rounds, sub { count($re, $text) });
    printf "%s %d %d %.2f\n", $name, $n, $total, $us;
}
# the name field: one search, eight names and a ! by mistake
# 名前の欄：探すのは1回。名前が8個と、まちがえて入った !
my $field = "Mira Ruby Lam GHC Perl Mira Ruby Lam!";
my ($us, $found) = clock($rounds, sub { $field =~ /^(\w+\s?)+$/ ? 1 : 0 });
printf "field %d 0 %.2f\n", $found, $us;
