
# Naive solution, brute force.
#
# https://oeis.org/A335065, but the square numbers of the sequence

sub is-progressive($n) {
    my $d = 2;
    while ($d * $d < $n) {
        # By proof on OEIS page, can assume r < d < q
        my $q = $n div $d;
        my $r = $n mod $d;
        say ($r, $d, $q, $n, sqrt($n)) if $d * $d == $r * $q;
        return True if $d * $d == $r * $q;
        $d += 1;
    }
    False
}

my $total = 0;
#for 1..316 -> $i { # 316 = floor(sqrt(100,000))
for 1..1000 -> $i { # 1000 = a convenient number for testing :)
#for 1..3162277 -> $i { # 3162277 = floor(sqrt(10,000,000,000,000))
    #say $i if $i mod 100 == 0;
    my $candidate = $i * $i;
    if (is-progressive($candidate)) {
        $total += $candidate;
    }
}
say $total;
