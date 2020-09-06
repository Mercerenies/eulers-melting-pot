.sub foo
    .local pmc all_sums
    .local pmc all_lengths
    .local pmc visited
    new all_sums, "Hash"
    new all_lengths, "Hash"

    $I0 = 0
INIT_LOOP:
    if $I0 > 1000000 goto INIT_DONE
    all_sums[$I0] = 0
    all_lengths[$I0] = 0
    $I0 += 1
    goto INIT_LOOP
INIT_DONE:

    $I1 = 1
SUM_LOOP:
    if $I1 > 1000000 goto SUM_DONE

    $I2 = 2 * $I1
SUM_INNER_LOOP:
    if $I2 > 1000000 goto SUM_INNER_DONE
    $I3 = all_sums[$I2]
    $I4 = $I3 + $I1
    all_sums[$I2] = $I4
    $I2 += $I1
    goto SUM_INNER_LOOP
SUM_INNER_DONE:

    $I1 += 1
    goto SUM_LOOP
SUM_DONE:

    $I5 = 1
LENGTH_LOOP:
    if $I5 > 1000000 goto LENGTH_DONE
    $I6 = $I5
    new visited, "Hash"
    $I8 = 0

LENGTH_INNER_LOOP:
    $I7 = visited[$I6]
    if $I7 > 0 goto LENGTH_INNER_DONE

    visited[$I6] = 1
    $I6 = all_sums[$I6]
    $I8 += 1

    if $I6 <= 0 goto LENGTH_INNER_DONE
    if $I6 > 999999 goto LENGTH_INNER_DONE
    if $I6 < $I5 goto LENGTH_INNER_DONE

    goto LENGTH_INNER_LOOP
LENGTH_INNER_DONE:

    if $I6 != $I5 goto DONT_WRITE
    all_lengths[$I5] = $I8
DONT_WRITE:

    $I5 += 1
    goto LENGTH_LOOP
LENGTH_DONE:

    $I9 = 0
    $I10 = 0
    $I11 = 1
BEST_LOOP:
    if $I11 > 1000000 goto BEST_END

    $I12 = all_lengths[$I11]
    if $I12 <= $I9 goto BEST_CONTINUE
    $I9 = $I12
    $I10 = $I11

BEST_CONTINUE:
    $I11 += 1
    goto BEST_LOOP
BEST_END:

    print $I10
    print "\n"

.end
