
# problem184_3.raku with a lot of the complexities boiled down, for
# simplicity. Unused functions removed, etc.

class Point {
    has Int $.x;
    has Int $.y;

    multi method Str() { "($!x, $!y)" }
}

sub other-limit-for($radius, $y) {
    ceiling(sqrt($radius * $radius - $y * $y));
}

sub count-quads($radius, $points-in-one-quadrant, $a) {
    my $b-lines = 0; # Above the origin
    my $c-lines = 0; # Below the origin
    for (0..^$radius).reverse -> $by {
        my $b-limit = other-limit-for($radius, $by);
        my $b-pivot = ($a.x * $by) / $a.y;
        if $b-pivot < 1 {
            $b-lines += ($b-limit - 1);
        } elsif $b-pivot > $b-limit - 1 {
            $c-lines += ($b-limit - 1);
        } elsif $b-pivot.denominator == 1 {
            $b-lines += ($b-limit - $b-pivot - 1);
            $c-lines += ($b-pivot - 1);
        } else {
            $b-lines += ($b-limit - $b-pivot.ceiling);
            $c-lines += $b-pivot.floor;
        }
    }
    $b-lines * ($c-lines + $points-in-one-quadrant);
}

sub run($radius) {
    my $points-in-one-quadrant = 0;
    my $outerlimit = $radius;
    for 0..^$radius -> $y {
        $points-in-one-quadrant += other-limit-for($radius, $y) - 1;
    }

    my $count = 0;
    for 1..^$radius -> $ax {
        my $alimit = other-limit-for($radius, $ax);
        for 1..^$alimit -> $ay {
            my $a = Point.new(x => $ax, y => $ay);
            $count += count-quads($radius, $points-in-one-quadrant, $a);
        }
    }
    $count * 4;
}

say run(105);
