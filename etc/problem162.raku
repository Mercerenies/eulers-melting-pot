
# Simple combinatorial solution. For each number of digits, count the
# ways to place A, 1, and 0 without worrying about the leading zero,
# then subtract the leading zero solutions.

sub postfix:<!>($n) {
    [*] 1..$n
}

sub ncr($n, $k) {
    $n! div ($k! × ($n - $k)!)
}

sub multinomial($n, *@ks) {
    my $ks-sum = [+] @ks;
    my $total = $n! div ($n - $ks-sum)!;
    for @ks -> $k {
        $total div= $k!;
    }
    return $total;
}

sub count-for($digits, $zeroes, $ones, $as) {
    # The number of ways to place this many zeroes, ones, and A's,
    # without regard for leading zeroes. This overcounts, and we have
    # to remove the leading zeroes in a moment.
    my $count = multinomial($digits, $zeroes, $ones, $as);
    # Now remove the leading zeroes.
    if $zeroes > 0 {
        $count -= multinomial($digits - 1, $zeroes - 1, $ones, $as);
    }
    # Multiply by the unused digits.
    $count *= 13 ** ($digits - $zeroes - $ones - $as);
    return $count;
}

my $final-count = 0;
for 1..16 -> $digits {
    for 1..$digits -> $zeroes {
        for 1..($digits-$zeroes) -> $ones {
            for 1..($digits-$zeroes-$ones) -> $as {
                $final-count += count-for($digits, $zeroes, $ones, $as);
            }
        }
    }
}
say $final-count.base(16);
