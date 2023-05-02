
sub sublists(@input) {
    sublists-rec(@input, 0).map(*.reverse);
}

sub sublists-rec(@input, $index) {
    return [$([])] if $index >= @input.elems;
    my @rec = sublists-rec(@input, $index + 1);
    @rec.append(@rec.map({ $^a.clone.push(@input[$index]) }));
    return @rec;
}

my $upper-limit = 80;

# See the long comments on problem152_1.raku for why we're doing this.
sub analyze-prime($p) {
    return Nil unless $p.is-prime and $p > 2;
    my @multiples = $p, $p + * ... $p + * > $upper-limit;
    gather {
        for sublists(@multiples) -> @candidate {
            my $sum = @candidate.map({ 1 / ($^a * $^a) }).sum;
            if ($sum.Rat.denominator % $p != 0) {
                take @candidate;
            }
        }
    }
}

my @result = analyze-prime(5);
say @result.elems;
.say for @result;
