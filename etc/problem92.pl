
use List::Util qw(sum);

my %cache;

my $total = 0;
for my $i (1..9999999) {
    my $j = $i;
    while (1) {
        $j = $cache{$j} if defined $cache{$j};
        last if $j == 1;
        ++$total and last if $j == 89;
        $j = sum map { $_ * $_ } split(//, $j);
    }
    $cache{$i} = $j;
}
print "$total\n";
