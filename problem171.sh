#!/bin/fish

set limit 1000000000

for i in $(seq 1621)
    set remaining_digit_sums_count[$i] 0
end
for i in $(seq 0 40)
    set array_index $(math $i x $i + 1)
    set remaining_digit_sums_count[$array_index] 1
end

# Now iteratively add the 11 digits.
for i in $(seq 11)
    for j in $(seq 1621)
        for digit in $(seq 9)
            set k $(math $j + $digit x $digit)
            # Note: If k is out of bounds, then
            # $remaining_digit_sums_count[$k] is "" and the + symbol
            # is unary.
            set remaining_digit_sums_count[$j] $(math $remaining_digit_sums_count[$k] + $remaining_digit_sums_count[$j])
        end
    end
end

# Note: The magic number 40320 that appears in several places below is
# just `8!`, the numerator of our multinomial.

set final_total 0
for ones in $(seq 0 9)
    for twos in $(seq 0 $(math 9 - $ones))
        for threes in $(seq 0 $(math 9 - $ones - $twos))
            for fours in $(seq 0 $(math 9 - $ones - $twos - $threes))
                for fives in $(seq 0 $(math 9 - $ones - $twos - $threes - $fours))
                    for sixes in $(seq 0 $(math 9 - $ones - $twos - $threes - $fours - $fives))
                        for sevens in $(seq 0 $(math 9 - $ones - $twos - $threes - $fours - $fives - $sixes))
                            for eights in $(seq 0 $(math 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens))
                                for nines in $(seq 0 $(math 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights))
                                    set array_index $(math 1 + 1 x $ones + 4 x $twos + 9 x $threes + 16 x $fours + 25 x $fives + 36 x $sixes + 49 x $sevens + 64 x $eights + 81 x $nines)
                                    set arrangements $(math $remaining_digit_sums_count[$array_index] % $limit)
                                    for position_in_number in $(seq 0 8)
                                        # Ones
                                        if test $ones -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones - 1 ')' x fac '(' $twos ')' x fac '(' $threes ')' x fac '(' $fours ')' x fac '(' $fives ')' x fac '(' $sixes ')' x fac '(' $sevens ')' x fac '(' $eights ')' x fac '(' $nines ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 1 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                        # Twos
                                        if test $twos -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones ')' x fac '(' $twos - 1')' x fac '(' $threes ')' x fac '(' $fours ')' x fac '(' $fives ')' x fac '(' $sixes ')' x fac '(' $sevens ')' x fac '(' $eights ')' x fac '(' $nines ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 2 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                        # Threes
                                        if test $threes -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones ')' x fac '(' $twos ')' x fac '(' $threes - 1 ')' x fac '(' $fours ')' x fac '(' $fives ')' x fac '(' $sixes ')' x fac '(' $sevens ')' x fac '(' $eights ')' x fac '(' $nines ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 3 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                        # Fours
                                        if test $fours -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones ')' x fac '(' $twos ')' x fac '(' $threes ')' x fac '(' $fours - 1 ')' x fac '(' $fives ')' x fac '(' $sixes ')' x fac '(' $sevens ')' x fac '(' $eights ')' x fac '(' $nines ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 4 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                        # Fives
                                        if test $fives -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones ')' x fac '(' $twos ')' x fac '(' $threes ')' x fac '(' $fours ')' x fac '(' $fives - 1 ')' x fac '(' $sixes ')' x fac '(' $sevens ')' x fac '(' $eights ')' x fac '(' $nines ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 5 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                        # Sixes
                                        if test $sixes -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones ')' x fac '(' $twos ')' x fac '(' $threes ')' x fac '(' $fours ')' x fac '(' $fives ')' x fac '(' $sixes - 1 ')' x fac '(' $sevens ')' x fac '(' $eights ')' x fac '(' $nines ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 6 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                        # Sevens
                                        if test $sevens -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones ')' x fac '(' $twos ')' x fac '(' $threes ')' x fac '(' $fours ')' x fac '(' $fives ')' x fac '(' $sixes ')' x fac '(' $sevens - 1 ')' x fac '(' $eights ')' x fac '(' $nines ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 7 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                        # Eights
                                        if test $eights -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones ')' x fac '(' $twos ')' x fac '(' $threes ')' x fac '(' $fours ')' x fac '(' $fives ')' x fac '(' $sixes ')' x fac '(' $sevens ')' x fac '(' $eights - 1 ')' x fac '(' $nines ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 8 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                        # Nines
                                        if test $nines -gt 0
                                            set final_total $(math '(' $final_total + '(' '(' '(' '(' '(' 40320 / '(' fac '(' $ones ')' x fac '(' $twos ')' x fac '(' $threes ')' x fac '(' $fours ')' x fac '(' $fives ')' x fac '(' $sixes ')' x fac '(' $sevens ')' x fac '(' $eights ')' x fac '(' $nines - 1 ')' x fac '(' 9 - $ones - $twos - $threes - $fours - $fives - $sixes - $sevens - $eights - $nines ')' ')' ')' % $limit ')' x '(' 9 x 10 ^ $position_in_number ')' ')' % $limit ')' x $arrangements ')' % $limit ')' % $limit)
                                        end
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end
end
echo $final_total
