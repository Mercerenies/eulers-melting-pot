#!/usr/bin/perl

use strict;
use warnings;
use 5.010;

my @defs;
my $main;

my $first = <>;
chomp $first;
$first =~ /N=(\d*)/;
my $n = $1;

my $name = undef;
my $curr = "";
while (<>) {
    chomp;
    if (/^:: (.*)/) {
        if (defined $name) {
            if ($name eq 'main') {
                $main = $curr;
            } else {
                unshift @defs, "$name=$curr";
            }
        }
        $name = $1;
        $curr = "";
    } else {
        $curr .= $_;
    }
}
if (defined $name) {
    if ($name eq 'main') {
        $main = $curr;
    } else {
        unshift @defs, "$name=$curr";
    }
}

local $" = ';';
print <<"RESULT";
[$main] [@defs] ^ . $n @
RESULT
