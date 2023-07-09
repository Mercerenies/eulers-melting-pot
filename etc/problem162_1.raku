
# Same as problem162.raku, but rephrased to be more like Excel's
# multinomial function.

sub postfix:<!>($n) {
    [*] 1..$n
}

sub ncr($n, $k) {
    $n! div ($k! × ($n - $k)!)
}

sub multinomial(*@ks) {
    my $ks-sum = [+] @ks;
    my $total = $ks-sum!;
    for @ks -> $k {
        $total div= $k!;
    }
    return $total;
}

sub count-for($wildcards, $zeroes, $ones, $as) {
    # The number of ways to place this many zeroes, ones, and A's,
    # without regard for leading zeroes. This overcounts, and we have
    # to remove the leading zeroes in a moment.
    my $count = multinomial($wildcards, $zeroes, $ones, $as);
    # Now remove the leading zeroes.
    if $zeroes > 0 {
        $count -= multinomial($wildcards, $zeroes - 1, $ones, $as);
    }
    # Multiply by the unused digits.
    $count *= 13 ** $wildcards;
    return $count;
}

my $final-count = 0;
for 0..16 -> $wildcards {
    my $sum = 0;
    for 1..16-$wildcards -> $zeroes {
        for 1..(16-$wildcards-$zeroes) -> $ones {
            for 1..(16-$wildcards-$zeroes-$ones) -> $as {
                $final-count += count-for($wildcards, $zeroes, $ones, $as);
                $sum += count-for($wildcards, $zeroes, $ones, $as);
            }
        }
    }
    say "$wildcards {$sum % (16 ** 6)}"
}
say $final-count.base(16);
