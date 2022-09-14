
# Naive solution, brute force.
#
# https://oeis.org/A335065

sub is-progressive($n) {
    for 2..^$n -> $d {
        my $q = $n div $d;
        my $r = $n mod $d;
        my ($a, $b, $c) = ($d, $q, $r).sort;
        return True if $b * $b == $a * $c;
    }
    False
}

my $total = 0;
for 1..316 -> $i { # 316 = floor(sqrt(100,000))
    my $candidate = $i * $i;
    if (is-progressive($candidate)) {
        say $candidate;
        $total += $candidate;
    }
}
say $total;
