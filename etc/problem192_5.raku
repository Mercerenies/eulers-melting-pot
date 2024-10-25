
# Distilling down problem192_4.hs to simpler terms (and hopefully
# improving performance while we're at it).
#
# Correct, but MUCH slower. 3 minutes 40 seconds

constant LIMIT = 1_000_000_000_000;

# A finite continued fraction represented as a Raku sequence.
class FiniteFrac {
    has @.seq;
    has $.index is rw;
    has $.delta;
    has $.lower-bound;
    has $.upper-bound;

    method new(@seq, :$index = 0, :$delta = 1, :$lower-bound = 0, :$upper-bound = @seq.elems) {
        self.bless(:@seq, :$index, :$delta, :$lower-bound, :$upper-bound)
    }

    method next {
        if $.lower-bound <= $.index < $.upper-bound {
            my $old-index = $.index;
            $.index += $.delta;
            @.seq[$old-index]
        } else {
            Nil
        }
    }
}

class State {
    has $.n;
    has $.a0;
    has $.r is rw;
    has $.s is rw;
    has $.a is rw;

    method next {
        my $last-a = $.a;
        my $r = $.a * $.s - $.r;
        my $s = ($.n - $r * $r) div $.s;
        my $a = ($.a0 + $r) div $s;
        $.r = $r;
        $.s = $s;
        $.a = $a;
        $last-a
    }

    submethod initial($n --> State) {
        my $a0 = int-sqrt($n);
        State.new(:$n, :$a0, :r(0), :s(1), :a($a0))
    }
}

# Compare two "next"able objects representing a continued fraction.
sub cmp($a, $b) {
    my $mult = 1;
    loop {
        my $a-term = $a.next;
        my $b-term = $b.next;
        return $mult * Order::More unless $a-term.defined;
        return $mult * Order::Less unless $b-term.defined;
        if $a-term != $b-term {
          return $mult * ($a-term <=> $b-term);
        }
        $mult *= -1;
    }
}

sub realize(@convergent) {
    my $accum = @convergent[*-1] / 1;
    for ((0 .. @convergent.elems - 2).reverse) -> $i {
      $accum = @convergent[$i] + 1 / $accum;
    }
    $accum
}

sub gen-last-convergent($n, $limit = LIMIT) {
    my @convergent;
    my $state = State.initial($n);
    @convergent.push($state.a);
    while realize(@convergent).denominator <= $limit {
      $state.next;
      @convergent.push($state.a);
    }
    (@convergent, $state)
}

sub find-best($n, $limit = LIMIT --> Rational) {
    my ($convergent, $state) = gen-last-convergent($n, $limit);
    my $original-a = $convergent[*-1];
    while 2 * $convergent[*-1] >= $original-a {
        my $value = realize($convergent);
        if 2 * $convergent[*-1] == $original-a {
            # Corner case for even convergents.
            $convergent[*-1] = $original-a;
            my $lhs = FiniteFrac.new($convergent, :index($convergent.elems - 1), :delta(-1), :lower-bound(1));
            if cmp($lhs, $state) > 0 {
                return $value if $value.denominator <= $limit;
            }
            last;
        }
        return $value if $value.denominator <= $limit;
        $convergent[*-1]--;
    }
    $convergent.pop;
    realize($convergent) # It's the only other candidate, so it must work.
}

sub int-sqrt($n) {
    $n.sqrt.floor
}

sub is-int-sqrt($n) {
    my $s = int-sqrt($n);
    $n == $s * $s;
}

my $sum = 0;
for (2..100_000) -> $n {
    say $n if $n %% 1000;
    next if is-int-sqrt($n);
    $sum += find-best($n).denominator;
}
say $sum;
