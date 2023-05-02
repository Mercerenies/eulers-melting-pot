
sub sublists(@input) {
    sublists-rec(@input, 0).map(*.reverse);
}

sub sublists-rec(@input, $index) {
    return [$([])] if $index >= @input.elems;
    my @rec = sublists-rec(@input, $index + 1);
    @rec.append(@rec.map({ $^a.clone.push(@input[$index]) }));
    return @rec;
}

sub all-sums(@input) {
    sublists(@input).map(*.sum);
}

my $upper-limit = 80;
my @possible-values = (2..$upper-limit);

# Let S be the space of sums of inverse squares in our solution space.
# Let p be an odd prime. Then S is partitioned into two subsets: S0
# containing those fractions which do *not* contain a factor of p in
# their denominator and S1 containing those which do.
#
# Now, if we take a fraction a/b (in reduced form) from S0 (so p does
# not divide b) and add a quantity 1/x^2 where p does not divide x,
# then we get (ax^2 + b)/(x^2 b). The denominator is x^2 b, a product
# of two things not divisible by p, so the denominator is not
# divisible by p. Hence, if we take something in S0 and add an inverse
# square which is not a multiple of p, we get something in S0.
#
# Likewise, suppose a/b (in reduced form) is in S1. Then p divides b
# and p does not divide a. Suppose 1/x^2 where p does not divide x.
# Then the sum is (ax^2 + b)/(x^2 b). The denominator has a b in it,
# hence p divides it. The only way this fraction can simplify to
# something in S0 is if the numerator is also divisible by p. But p
# divides b and p does not divide ax^2, so p does not divide the
# numerator, hence our sum is in S1.
#
# All this is to say that the only way to "move" between S0 and S1 is
# by adding an inverse square of a multiple of p. Anything which is
# not a multiple of p does not affect the "has a factor of p"
# property. Our desired sum (1/2) is in S0, and the empty sum (the
# starting point of our search, 0) is in S0 as well.
#
# It follows that, if we add a factor of p at some point to the
# denominator, we must remove it later by adding something else with a
# factor of p to the denominator. The only way the multiples of p can
# "cancel off" is if their sum has no fraction of p in its
# denominator.
#
# So, for each odd prime p >= 5 (we don't do p = 3 because it would
# take too long to brute force), we check if there are any nonempty
# sums of inverse squares of multiples of p which do not have a p in
# the denominator. If not, we can safely remove all multiples of p
# from consideration.
for (5..$upper-limit) -> $p {
    next unless $p.is-prime;
    my @multiples = $p, $p + * ... $p + * > $upper-limit;
    my @sums = all-sums(@multiples.map({ 1 / ($^a * $^a) }));
    unless @sums.map({ $^frac != 0 and $^frac.denominator % $p != 0 }).any {
        # We found a prime that can't possibly work! Remove all
        # multiples of it from the possible values list.
        @possible-values .= grep({ $^value % $p != 0 });
    }
}

my @inv-squares = @possible-values.map({ 1 / ($^a * $^a) });
my $half = 1/2;

my $lower-bound = $half - sum(@inv-squares);
my $upper-bound = $half;
my @sums = (0);
my $solution-count = 0;

say @possible-values;
say sum(@inv-squares);

for @inv-squares -> $square {
    $lower-bound += $square;
    @sums.append(gather {
        for @sums -> $sum {
            if ($sum + $square == $half) {
                $solution-count += 1;
            } else {
                take $sum + $square;
            }
        }
    });
    @sums .= grep({ $lower-bound <= $^sum <= $upper-bound });
    say "{sqrt(1/$square)} {@sums.elems}";
}

say $solution-count;
