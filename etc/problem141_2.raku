
my $upper-limit = 10_000_000_000_000;

my $cubes = ((1...*).map({$^a * $^a * $^a}) ...^ (* >= $upper-limit * $upper-limit)).Set;

# Remember, $r < $d < $q

# After some math, d^3 + r^2 = w^2 r, or d^3 = w^2 r - r^2

exit(1);

my $w = 1;
while (True) {
    say $w;
    last if $w * $w >= $upper-limit
}
