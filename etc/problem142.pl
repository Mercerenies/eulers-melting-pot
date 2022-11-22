
use strict;
use warnings;
use 5.010;

# We have x > y > z > 0 and what x + y, x - y, x + z, x - z, y + z, y
# - z all perfect squares.
#
# Parameterize: a = x + y, b = x - z, c = x - y.
#
# Then we find
# x = (a + c) / 2
# y = (a - c) / 2
# z = x - b = (a + c) / 2 - b
#
# The numbers we want to be perfect squares are now
# x + y = a
# x - y = c
# x + z = a - b + c
# x - z = b
# y + z = a - b
# y - z = b - c
#
# And also, with a bit of basic algebra, we translate the constraint x
# > y > z > 0 into the following two constraints:
# (1) a > b > c
# (2) a + c > 2 b
#
# So iterate over a, b, c perfect squares and check that (a - b + c, a
# - b, b - c) are perfect squares. For a given value of a, we know
#
# x + y + z = 3 a / 2 - b + c / 2 >= (3 a / 2) - a = a / 2
#
# So we iterate until we've found a solution AND a / 2 > our best
# solution so far.

sub is_perfect_square {
    my $x = shift;
    sqrt $x == int sqrt $x
}

my $best_solution;

my $a_sqrt = 3;
while (1) {
    my $a = $a_sqrt * $a_sqrt;
    last if defined($best_solution) && $a / 2 > $best_solution;
    my $b_sqrt = 2;
    while ($b_sqrt < $a_sqrt) {
        my $b = $b_sqrt * $b_sqrt;
        next unless is_perfect_square($a - $b);
        my $c_sqrt = 1;
        while ($c_sqrt < $b_sqrt) {
            my $c = $c_sqrt * $c_sqrt;
            next if ($a + $c) % 2 == 1;
            next if ($a + $c) <= 2 * $b;
            if (is_perfect_square($a - $b + $c) && is_perfect_square($b - $c)) {
                my $sum = (3 * $a + $c) / 2 - $b;
                if ((!defined($best_solution)) || ($best_solution > $sum)) {
                    $best_solution = $sum;
                }
            }
        } continue {
            $c_sqrt += 1;
        }
    } continue {
        $b_sqrt += 1;
    }
    $a_sqrt += 1;
}
say $best_solution;
