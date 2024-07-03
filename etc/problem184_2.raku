
# Analyze the quadrants. Consider a triangle ABC containing the origin
# in its interior. Up to rotational symmetry, we can assume WLOG that
# point A is either in Quadrant I or on the positive X axis.
#
# This gives the following configurations for points. For the purposes
# of this discussion, axes belong to their counter-clockwise quadrant.
# So the +X axis belongs to Quadrant I, +Y to Quadrant II, -X to III,
# and -Y to IV.
#
# case # |  A  |  B  |  C
#    1   |  I  |  I  | III
#    2   |  I  | II  | III
#    3   |  I  | II  | IV
#    4   |  I  | III | II
#    5   |  I  | III | III
#    6   |  I  | III | IV
#
# Case (4) is equivalent to (2) by swapping the points B and C. (2)
# and (3) are equivalent to (6) under rotational symmetry. Likewise,
# (I) is equivalent to (5) under rotational symmetry. So that leaves
# only two cases: (5) and (6).
#
# That is, we may assume A is in Quadrant I, B is in Quadrant III, and
# C is in either Quadrant III or IV. With that in mind, and still
# assuming the origin is contained in the interior, it follows that
# the line AB must have positive Y intercept. (If it does not, then C
# is in Quadrant III, and we can swap B and C to get a solution where
# AB has positive Y intercept)
#
# With these additional assumptions, we can also rule out cases where
# A or C is on the X axis. If A is on the (positive) X axis and B is
# in Quadrant III, then the AB line has negative Y intercept,
# contradiction. If C is on the (negative) X axis, then ABC must be
# entirely above the origin.
#
# Beyond this, just count them, cleverly short-circuiting where
# possible. Every solution we find with this algorithm counts as 4
# solutions by rotational symmetry.
#
# If C is in Quadrant IV (including the negative Y axis) and ABC
# satisfies the above conditions, then ABC definitely includes the
# origin, so we can count those points in bulk.

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

sub run($radius) {
    my $points-in-one-quadrant = points($radius).elems;
    my $count = 0;
    for 1..^$radius -> $ax {
        my $alimit = other-limit-for($radius, $ax);
        for 1..^$alimit -> $ay {
            my $a = Point.new(x => $ax, y => $ay);
            for (0..^$radius).reverse -> $by {
                my $blimit = other-limit-for($radius, $by);
                next if y-intercept($a, Point.new(x => $blimit, y => - $by)) <= 0;
                for (1..^$blimit).reverse -> $bx {
                    my $b = Point.new(x => - $bx, y => - $by);
                    next if y-intercept($a, $b) <= 0;

                    # First, count all of the $c points in Quadrant IV
                    # (all such points form a valid triangle)
                    $count += $points-in-one-quadrant;

                    # Now count valid points in Quadrant III
                    for (1..^$radius) -> $cy {
                        my $climit = other-limit-for($radius, $cy);
                        for (1..^$climit) -> $cx {
                            my $c = Point.new(x => - $cx, y => - $cy);
                            $count++ if $b.y * $a.x > $a.y * $b.x && $c.y * $b.x > $b.y * $c.x && $a.y * $c.x > $c.y * $a.x;
                        }
                    }
                }
            }
        }
    }
    $count * 4
}

for 1..15 -> $r {
    say "$r {run($r)}";
}
