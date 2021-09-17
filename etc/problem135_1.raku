
my @solutions = 0 xx 1000000;
my $limit = 999999;

for (1..$limit) -> $a {
    for (1..$limit) -> $b {
        last if $a * $b > $limit;
        next if 3 * $b <= $a;
        next unless ($a + 5 * $b) % 4 == 0;
        @solutions[$a * $b]++;
    }
}
say (@solutions «==» 10).sum;
