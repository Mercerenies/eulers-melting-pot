
# Same as problem183.raku, but I'm eliminating the dependency on
# rational number arithmetic, since we can do the rational part
# ourselves. All we'll need is a good floating-point engine for the >
# check.

# For the function f(x) = (n / x)^x (for positive real x), basic
# calculus tells us that the unique maximum is achieved at x = n / e,
# and that the function is increasing before that point and decreasing
# after. So the best integer k to maximize (n / k)^k must either be
# floor(x) or ceil(x). Try both and pick the better of the two.
sub find-best-k($n --> Int) {
    # $x and $x+1 are the two candidates for maxima.
    my $x = ($n / e).floor;
    # A bit of algebra tells us this is equivalent to checking whether
    # f($x) > f($x + 1). Writing it this way prevents floating-point
    # overflow, whereas the naive inequality just resolves both sides
    # to Infinity.
    if ($x + 1) * ($x + 1) ** (1 / $x) / $n ** (1 / $x) > $x {
        return $x;
    } else {
        return $x + 1;
    }
}

sub is-terminating-decimal($denom is copy) {
    # Remove all of the 2's and 5's.
    while $denom % 2 == 0 {
        $denom div= 2;
    }
    while $denom % 5 == 0 {
        $denom div= 5;
    }
    # We are a terminating decimal iff there's nothing left.
    return $denom == 1;
}

my $sum = 0;
for 5 .. 10000 -> $n {
    my $k = find-best-k($n);
    # For the purposes of checking whether we're a terminating
    # decimal, we just need n / k, not (n / k)^k. Raising a rational
    # number to a positive integer power does not change the prime
    # factors present in the numerator or denominator (merely changes
    # their multiplicity), so n / k can be written with only 2's and
    # 5's if and only if (n / k)^k can.
    if is-terminating-decimal($k div ($n gcd $k)) {
        $sum -= $n;
    } else {
        $sum += $n;
    }
}
say $sum;
