
# Not trying to enumerate all of them, just looking for a pattern at
# the moment.
#
# Immediate observations, right off the bat.
#
# 1. If the last digit is even, then the second-to-last digit must be
# even as well, since all multiples of an even number are even.
#
# 2. The last digit cannot be zero. Proof by contradiction: If the
# last digit of n (the original number) is zero, then the last digit
# of m (the shifted number) must also be zero in order for it to be a
# multiple. Hence, n ends in two zeroes. But then m must end in two
# zeroes. And thus n must have three zeroes, and so on. So the only
# number with this right-shift property which ends in zero is zero
# itself.
#
# 3. Obviously, the number must not get smaller when we shift. This
# means that the rightmost digit must be greater than or equal to the
# leftmost digit.
#
# 4. Obviously, a number which is simply the same digit repeated
# several times has the right-shift property, since shifting it does
# not change its value.

sub do-shift($n) {
    + (substr($n, * - 1) ~ substr($n, 0, * - 1))
}

sub has-right-rotation-property($n) {
    my $m = do-shift($n);
    $m % $n == 0
}

for (10..10000000) -> $n {
    if (has-right-rotation-property($n)) {
        my $multiplicand = do-shift($n) div $n;
        # For now, ignore the trivial answers which are just the same
        # digit repeated.
        if ($multiplicand != 1) {
            say "$n $multiplicand";
        }
    }
}
