
# Ignore this file I made a logic error :(

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
    fail unless $p.is-prime and $p > 2;
    my @multiples = $p, $p + * ... $p + * > $upper-limit;
    gather {
        for sublists(@multiples) -> @candidate {
            my $sum = @candidate.map({ 1 / ($^a * $^a) }).sum;
            if ($sum.Rat.denominator % $p != 0) {
                take $sum;
            }
        }
    }
}

my @candidates = 2..80;
my $targets = BagHash.new: 1/2 => 1;

for (5..80) -> $p {
    next unless $p.is-prime;
    my $sums = analyze-prime($p);
    my $old-targets = $targets.clone;
    for @$sums -> $sum {
        for %$old-targets -> $target, $weight {
            $targets{$target - $sum} += $weight;
        }
    }
    last;
}

say $targets;


