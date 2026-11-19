# Perl's cache of places it has tried (the VM chapter) is turned off
# when the pattern has a back reference: the steps grow again.
# Perl が試した場所を覚えておく工夫（VM の章）は、パターンに後方参照が
# あると、使われない。手数が、また増えていく
use strict;
use warnings;
use Time::HiRes qw(time);

# ANCHOR: noref
# Perl: the name field, without and with \1 at the end
# Perl：名前の欄。最後の \1 がないときと、あるとき
my @names = qw(Mira Ruby Lam GHC Perl);
for my $n (6, 7, 8) {
    my $field = join(" ", map { $names[$_ % 5] } 0 .. $n - 1) . "!";
    for my $pattern (qr/^(\w+\s?)+$/, qr/^(\w+\s?)+\1$/) {
        my $start = time;
        my $found = $field =~ $pattern ? "found" : "not found";
        printf "%d %-20s %s, %.2fs\n", $n, $pattern, $found, time - $start;
    }
}
# ANCHOR_END: noref
