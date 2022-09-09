
# Rearranged some if statements, got it down to 23 sec.

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
        my $a = $m * $m - $n * $n;
        my $b = 2 * $m * $n;
        my $c = $m * $m + $n * $n;
        if ($c % ($b - $a) == 0) {
            if (gcd($m, $n) == 1) {
                my $total-triples = 99999999 div (2 * $m * ($m + $n));
                $count += $total-triples;
            }
        }
        $m += 2;
    }
}

say $count;
