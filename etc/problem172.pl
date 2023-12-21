#!/usr/bin/perl

# So we need bignums, and Perl's bignums are too slow.

use strict;
use warnings;
use 5.010;

use bignum;

# Combinatorial solution to Problem 172.

sub fac {
    my ($n) = @_;
    my $result = 1;
    for my $i (1..$n) {
      $result *= $i;
    }
    return $result;
}

sub ncr {
    my ($n, $r) = @_;
    return fac($n) / (fac($r) * fac($n - $r));
}

sub multinomial {
    my $n = shift;
    my $remainder = $n;
    my $total = fac($n);
    for my $i (@_) {
      $total /= fac($i);
      $remainder -= $i;
    }
    $total /= fac($remainder);
    return $total;
}

my $total = 0;
for my $ones (0..3) {
    for my $twos (0..3) {
        for my $threes (0..3) {
            for my $fours (0..3) {
                for my $fives (0..3) {
                    for my $sixes (0..3) {
                        say "$ones $twos $threes $fours $fives $sixes";
                        for my $sevens (0..3) {
                            next if $ones + $twos + $threes + $fours + $fives + $sixes + $sevens > 18;
                            for my $eights (0..3) {
                                next if $ones + $twos + $threes + $fours + $fives + $sixes + $sevens + $eights > 18;
                                for my $nines (0..3) {
                                    my $total_nonzero_digits = $ones + $twos + $threes + $fours + $fives + $sixes + $sevens + $eights + $nines;
                                    next if $total_nonzero_digits > 18;
                                    for my $zeroes (0..3) {
                                        my $total_digits = $total_nonzero_digits + $zeroes;
                                        my $total_numbers = multinomial($total_digits, $ones, $twos, $threes, $fours, $fives, $sixes, $sevens, $eights, $nines);
                                        my $total_with_leading_zeroes = ($zeroes > 0) ? multinomial($total_digits - 1, $ones, $twos, $threes, $fours, $fives, $sixes, $sevens, $eights, $nines) : 0;
                                        $total += ($total_numbers - $total_with_leading_zeroes);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
say $total;
