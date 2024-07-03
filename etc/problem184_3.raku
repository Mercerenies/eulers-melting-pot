
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

sub count-quad-iv($radius, $points-in-one-quadrant, $a) {
    my $count = 0;
    for (0..^$radius).reverse -> $by {
        my $blimit = other-limit-for($radius, $by);
        for (1..^$blimit).reverse -> $bx {
            my $b = Point.new(x => - $bx, y => - $by);
            next if y-intercept($a, $b) <= 0;
            $count += $points-in-one-quadrant;
        }
    }
    $count;
}

sub count-quad-iii($radius, $a) {
    my $b-lines = 0; # Above the origin
    my $c-lines = 0; # Below the origin
    for (0..^$radius).reverse -> $by {
        my $blimit = other-limit-for($radius, $by);
        for (1..^$blimit).reverse -> $bx {
            my $b = Point.new(x => - $bx, y => - $by);
            my $y-intercept = y-intercept($a, $b);
            $b-lines++ if $y-intercept > 0;
            $c-lines++ if $y-intercept < 0;
        }
    }
    $b-lines * $c-lines;
}

sub run($radius) {
    my $count = 0;
    my $points-in-one-quadrant = points($radius).elems;
    for 1..^$radius -> $ax {
        my $alimit = other-limit-for($radius, $ax);
        for 1..^$alimit -> $ay {
            say "$ax $ay";
            my $a = Point.new(x => $ax, y => $ay);
            $count += count-quad-iv($radius, $points-in-one-quadrant, $a);
            $count += count-quad-iii($radius, $a);
        }
    }
    $count * 4;
}

#for 1..40 -> $r {
#    say "$r {run($r)}";
#}

say run(105);
