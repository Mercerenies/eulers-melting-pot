
# Same as problem183_1.raku, written without functions and other
# things that Pyramid Scheme lacks.

my $sum = 0;
my $x = 1;
for 5 .. 10000 -> $n {
    my $k;
    while 2.718281828 * ($x + 1) < $n {
        $x += 1;
    }
    if ($x + 1) * ($x + 1) ** (1 / $x) / $n ** (1 / $x) > $x {
        $k = $x;
    } else {
        $k = $x + 1;
    }
    my $a = $n;
    my $b = $k;
    while $b != 0 {
        my $tmp = $a;
        $a = $b;
        while $tmp >= $b {
            $tmp -= $b;
        }
        $b = $tmp;
    }
    my $denom = $k div $a;

    loop {
        my $twomod = $denom;
        while $twomod >= 20 {
            $twomod -= 20;
        }
        while $twomod >= 2 {
            $twomod -= 2;
        }
        if $twomod == 0 {
            $denom div= 2;
        } else {
            last;
        }
    }

    loop {
        my $fivemod = $denom;
        while $fivemod >= 50 {
            $fivemod -= 50;
        }
        while $fivemod >= 5 {
            $fivemod -= 5;
        }
        if $fivemod == 0 {
            $denom div= 5;
        } else {
            last;
        }
    }

    if $denom == 1 {
        $sum -= $n;
    } else {
        $sum += $n;
    }
}
say $sum;
