#!/usr/bin/perl

use strict;
use warnings;
use 5.010;

my $var = 'c';
my $lastvar = 'b';
my $lastlastvar = 'a';
my $varlist = 'a, b';
for my $idx (3..12) {
    my $next = $idx + 1;
    print <<"END";
def dice$idx($varlist):
    return 0 if leq($lastvar, $lastlastvar) else do_10(dice$next($varlist, 0), dice$next($varlist, 1), dice$next($varlist, 2), dice$next($varlist, 3), dice$next($varlist, 4), dice$next($varlist, 5), dice$next($varlist, 6), dice$next($varlist, 7), dice$next($varlist, 8), dice$next($varlist, 9))

END
    $varlist = $varlist . ', ' . $var;
    $lastlastvar = $lastvar;
    $lastvar = $var;
    $var++;
}
