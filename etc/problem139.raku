
# The length of a side of the inner square is (b-a), so we want
# squares where (b-a) divides evenly into c.
#
# Perimeter must be less than 100,000,000, hence a + b + c <=
# 100,000,000
#
# Using Euclid's formula for generating primitive Pythagorean triples.
# That is, let 0 < n < m with m and n coprime and one of them even.
#
# a = m^2 - n^2, b = 2mn, c = m^2 + n^2
#
# Perimeter of resulting rectangle is a + b + c = 2 m (m + n)
#
# (b - a) = n^2 + 2mn - m^2
#
# Four minutes :)

sub gcd($a is copy, $b is copy) {
    while ($b != 0) {
        ($a, $b) = ($b, $a % $b);
    }
    $a
}

my $count = 0;
# 100,000,000 > perimeter = 2 m (m + n) >= 4 n^2 (since m > n), so n <= 5000.
for (1..5000) -> $n {
    my $m = $n + 1;
    while (2 * $m * ($m + $n) < 100000000) {
        if ((gcd($m, $n) == 1) && (($m + $n) % 2 == 1)) {
            my $a = $m * $m - $n * $n;
            my $b = 2 * $m * $n;
            my $c = $m * $m + $n * $n;
            if ($c % ($b - $a) == 0) {
                my $total-triples = 99999999 div (2 * $m * ($m + $n));
                $count += $total-triples;
            }
        }
        $m += 1;
    }
}

say $count;
