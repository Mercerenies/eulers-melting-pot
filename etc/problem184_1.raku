
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
    my $outerlimit = $radius - 1;
    for (- $outerlimit)..$outerlimit -> $ay {
        for (- $outerlimit)..$outerlimit -> $bx {
            my $alimit = other-limit-for($radius, $ay) - 1;
            my $blimit = other-limit-for($radius, $bx) - 1;
            for (- $blimit)..$blimit -> $by {
                for (- $alimit)..$alimit -> $ax {
                    next if $by * $ax >= $ay * $bx;
                    for (- $outerlimit)..$outerlimit -> $cx {
                        my $climit = other-limit-for($radius, $cx) - 1;
                        for (- $climit)..$climit -> $cy {
                            if $cy * $bx < $by * $cx && $ay * $cx < $cy * $ax {
                                $count++;
                            }
                        }
                    }
                }
            }
        }
    }
    $count / 3;
}

for 1..10 -> $r {
    say "$r {run($r)}";
}
