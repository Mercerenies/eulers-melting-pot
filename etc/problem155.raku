
# Direct computation: Too slow.

sub append(%map, $c, $n) {
    # If this capacitance is not present in the map, add it. If it is,
    # take the minimum of the two circuit sizes.
    %map{$c} = min(%map{$c} // Inf, $n);
}

sub up-to(Int $n) {
    # %distinct is a mapping from a capacitance (rational number) to
    # the minimum number of capacitors needed to produce that value.
    # Initial value: a single capacitor forms a circuit with base
    # capacitance 1.
    my $distinct = :{ (1/1) => 1 };
    for (2..$n) {
        my $new-distinct = :{};
        for $distinct.kv -> $c1, $n1 {
            for $distinct.kv -> $c2, $n2 {
                next if $n1 + $n2 > $n;
                # Do it in parallel and also in series.
                my $parallel = $c1 + $c2;
                my $series = 1 / (1 / $c1 + 1 / $c2);
                append($new-distinct, $parallel, $n1 + $n2);
                append($new-distinct, $series, $n1 + $n2);
            }
        }
        # Integrate back into $distinct
        for $new-distinct.kv -> $c, $n {
            append($distinct, $c, $n);
        }
    }
    $distinct.elems
}

say up-to($_) for 1..9;
