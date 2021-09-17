
# The equation in question is
#
# x^2 - y^2 - z^2 = n
#
# Assuming y < x, z can be written as
#
# z = y - (x - y) = 2y - x
#
# Hence, the equation is
#
# x^2 - y^2 - (2y-x)^2 = n
# - 5y^2 + 4yx = n
# y (4x - 5y) = n
#
# So we factor n and check all of its factor pairs for solutions.
#
# Note z must be positive for a solution to be valid, hence
#
# 2y - x > 0
# 2B - (A + 5B)/4 > 0
# 3B > A
#
# Also, x > y,
#
# A > -B
#
# Substitutions
#
# B = y
# A = 4x-5y
#
# (A + 5B)/4 = x
# B = y

sub factor_pairs($n) {
    gather {
        for (1..$n) -> $i {
            last if ($n div $i) * 3 <= $i; # 3 * B <= A
            take ($i, ($n div $i)) if $n % $i == 0;
        }
    }
}

sub count_solutions($n) {
    my $total = 0;
    for (factor_pairs($n)) -> ($a, $b) {
        $total++ if ($a + 5 * $b) % 4 == 0;
    }
    $total
}

my $total_sum = 0;
for (1..999999) -> $i {
    $total_sum++ if count_solutions($i) == 10;
    say $i if $i % 1000 == 0;
}
say $total_sum;
