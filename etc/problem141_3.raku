
# With some help from
# https://www.mathblog.dk/project-euler-141investigating-progressive-numbers-n-which-are-also-square/
#
# Can assume r < d < q, since d and q interchangeable, and if q < r <
# d then we have the oblong numbers which are not squares.
#
# Let the geometric progression ratio be written as a/b, with
# gcd(a,b)=1. Then d = ra/b, q = ra^2/b^2.
#
# q is an integer, so b^2 divides ra^2, and gcd(a,b)=1, so b doesn't
# divide a. Hence b^2 doesn't divide a^2. So b^2 divides r. That is,
# write r = cb^2 for some b.
#
# Then write r = cb^2, d = cba, q = ca^2. And finally, n = dq + r =
# c^2 b a^3 + c b^2.

sub is-square($x) {
    sqrt($x).Int == sqrt($x)
}

my $found = SetHash.new();

my $limit = 1_000_000_000_000;

for (1..10_000) -> $a { # 10,000 = ceil(cbrt(1 trillion))
    say $a if $a % 100 == 0;
    for (1..^$a) -> $b { # $a > $b, since our progression is increasing
        next if $a gcd $b > 1;
        last if $b * $a * $a * $a + $b * $b >= $limit;
        my $c = 1;
        while (True) {
            my $n = $c * $c * $b * $a * $a * $a + $c * $b * $b;
            last if $n >= $limit;
            $found{$n} = 1 if is-square($n);
            $c += 1;
        }
    }
}

say $found.keys.sort.map(*.sqrt);

say [+] $found.keys;
