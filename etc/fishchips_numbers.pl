#/usr/bin/perl

# Small numerical constant generator for ><>\\.:. (Fish Chips and
# Mushy Peas).

use strict;
use warnings;
use 5.010;

sub digit {
    my $n = shift;
    return sprintf('%x', $n);
}

sub generate_nonnegative {
    my $n = shift;
    if ($n < 16) {
        return digit($n);
    } else {
        # Is it a perfect square?
        my $sqrt = int(sqrt($n));
        if ($sqrt * $sqrt == $n) {
            return generate_nonnegative($sqrt) . ':*';
        }

        # Try factoring out a manageable constant.
        for (my $i = 15; $i >= 2; $i--) {
            if ($n % $i == 0) {
                return generate_nonnegative($n / $i) . digit($i) . '*';
            }
        }

        # Otherwise, subtract 15 and try again. (Subtracting 15
        # decreases by the maximum amount and is guaranteed to at
        # least get us an even number)
        return generate_nonnegative($n - 15) . 'f+';
    }
}

sub generate {
    my $n = shift;
    my $result = generate_nonnegative(abs($n));
    if ($n < 0) {
        $result = '0' . $result . '-';
    }
    return $result;
}

if ($ARGV[0] eq '--all') {
    for (my $i = 0; $i < 200; $i++) {
        say($i . "\t" . generate($i));
    }
} else {
    say generate($ARGV[0]);
}
