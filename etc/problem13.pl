#!/usr/bin/perl

my $res;
for (my $lhs = 0; $lhs < 10; $lhs++) {
    for (my $rhs = 0; $rhs < 10; $rhs++) {
        $res = $lhs + $rhs;
        $res =~ s/.(.)/c\1/;
        print('s#(\([0-9]*\)' . $lhs . '+\([0-9]*\)' . $rhs . ')#(\1+\2)' . $res . '#g');
        print("\nt start\n");
    }
}
