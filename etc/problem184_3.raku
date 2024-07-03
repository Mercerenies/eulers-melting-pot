
# Do all of the problem184_2.raku calculations. Now split the entire
# iteration into two parts, based on whether C is in Quadrant IV
# (including the negative Y axis) or C is in Quadrant III (excluding
# all axes, per our prior justification).
#
# If C is in Quadrant IV, we just need to brute-force A and B values.
# If C is in Quadrant III, then the AB line needs to go above the
# origin and the AC line needs to go below. We can count these B and C
# values independently and multiply.
#
# Runs; works in 9minutes
#
# With some optimizations on count-quad-iii, down to 4 minutes.
#
# And with not being stupid (more optimizations), we're down to 2 seconds.

class Point {
    has Int $.x;
    has Int $.y;

    multi method Str() { "($!x, $!y)" }
}

# Exclusive bound
sub other-limit-for($radius, $y) {
    # We want x^2 + y^2 < r^2
    # So x < sqrt(r^2 - y^2)
    # Hence, x < ceil(sqrt(r^2 - y^2))
    #
    # This function can be used to get x bounds from y or vice versa,
    # since it's all symmetrical.
    ceiling(sqrt($radius * $radius - $y * $y));
}

sub y-intercept(Point $a, Point $b) {
  my $m = ($b.y - $a.y) / ($b.x - $a.x);
  $a.y - $m * $a.x;
}

sub points($radius) {
    my $outerlimit = $radius;
    gather {
        for 0..^$outerlimit -> $y {
            my $innerlimit = other-limit-for($radius, $y);
            for 1..^$innerlimit -> $x {
                take Point.new(:$x, :$y);
            }
        }
    }
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
    my $count = 0;
    my $points-in-one-quadrant = points($radius).elems;
    for 1..^$radius -> $ax {
        say $ax;
        my $alimit = other-limit-for($radius, $ax);
        for 1..^$alimit -> $ay {
            my $a = Point.new(x => $ax, y => $ay);
            $count += count-quads($radius, $points-in-one-quadrant, $a);
        }
    }
    $count * 4;
}

#for 1..40 -> $r {
#    say "$r {run($r)}";
#}

say run(105);
