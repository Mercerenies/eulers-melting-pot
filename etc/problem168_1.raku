
constant $upper-limit-digit-count = 100;
constant $modulo-value = 100000;

sub do-shift($n) {
    + (substr($n, * - 1) ~ substr($n, 0, * - 1))
}

# Produce the minimal n-parasitic number given k as a seed, as per the
# algorithm on Wikipedia.
# https://en.wikipedia.org/wiki/Parasitic_number
sub generate-parasitic-number($n, $k is copy) {
    my $digits = 1;
    while (do-shift($k) != $k * $n) {
        my $product = $n * $k;
        my $new-digit = substr($product, * - $digits, 1) // '0';
        $k = $new-digit ~ $k;
        $digits++;
    }
    $k
}

sub count-all-one-parasitic {
    my $sum = 0;
    for (1..9) -> $digit {
        # Count the "short" values (2 to 4 digits) (we exclude numbers
        # under 10, per the problem description)
        $sum += [+] $digit «x« (2..4);
        # Count the five-digit version times the 96 times we need to
        # include it.
        $sum += ($digit x 5) * 96;
        $sum %= $modulo-value;
    }
    $sum
}

sub count-all($n) {
    my $sum = 0;
    for ($n..9) -> $k {
        my $parasitic-number = generate-parasitic-number($n, $k);
        # We can get another parasitic number by concatenating an
        # existing one with itself, so we have to count this several
        # times. So count this number once for each time that we can
        # do that. Since we only care about the last five digits, the
        # actual digits don't change.
        my $repeat-count = $upper-limit-digit-count div $parasitic-number.chars;
        $sum += ($parasitic-number * $repeat-count) % $modulo-value;
    }
    $sum
}

my $sum = 0;

# The 1-parasitic numbers are simply those that consist of the same
# digit repeated forever. Count these separately, since they're the
# only parasitic numbers with fewer than five digits.
$sum += count-all-one-parasitic() % $modulo-value;

for (2..9) -> $n {
    $sum += count-all($n) % $modulo-value;
}

say($sum % $modulo-value);
