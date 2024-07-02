
class Point {
    has Int $.x;
    has Int $.y;
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

sub points($radius) {
    my $outerlimit = $radius - 1;
    gather {
        for (- $outerlimit)..$outerlimit -> $y {
            my $innerlimit = other-limit-for($radius, $y) - 1;
            for (- $innerlimit)..$innerlimit -> $x {
                take Point.new(:$x, :$y);
            }
        }
    }
}

sub run($radius) {
    my $count = 0;
    my @points = points($radius);
    for @points.combinations(3) -> ($a, $b, $c) {
        $count++ if $b.y * $a.x < $a.y * $b.x && $c.y * $b.x < $b.y * $c.x && $a.y * $c.x < $c.y * $a.x;
    }
    $count * 2
}

for 1..10 -> $r {
    say "$r {run($r)}";
}
