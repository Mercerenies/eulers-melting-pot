
my $upper-limit = 10_000_000_000;

my $squares = ((1...*).map({$^a * $^a}) ...^ (* >= $upper-limit)).Set;

# Remember, $r < $d < $q

my $total = 0;

my $r = 1;
while ($r + ($r + 1) * ($r + 2) < $upper-limit) {
    say $r if $r < 200 || $r % 100 == 0;
    my $d = $r + 1;
    while (True) {
        my $q = ($d * $d) div $r;
        my $n = $q * $d + $r;
        last if $n > $upper-limit;
        if ((($d * $d) % $r == 0) && ($n ∈ $squares)) {
            say $n;
            $total += $n;
        }
        $d += 1;
    }
    $r += 1;
}
say $total;
