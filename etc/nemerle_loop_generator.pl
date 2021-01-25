#!/usr/bin/perl

# Nemerle is being silly and throwing C# errors when I try to do
# recursion, which is fun, so we're going to do some codegen to get
# rid of the recursion :)

# Fortunately, didn't actually need this. Turns out Nemerle just has
# an irrational hatred of generics that aren't array[...], not
# recursion.

use strict;
use warnings;
use 5.010;

sub run {
    my $index = shift;
    my $inner = ($index == 9) ? "result[resultPos] = ToLong(data); resultPos++;\n" : run($index + 1);
    $inner =~ s/^/      /mg;
    my $body = <<"END";
for (mutable loop$index = 0; loop$index < 10; loop$index++) {
  index = $index;
  if (count < 0) {
  } else if (10 - index < count) {
  } else if (10 - index == count) {
    for (mutable i = 0; i < index; i++) {
      data[i] = repeated;
    }
    result[resultPos] = ToLong(data); resultPos++;
  } else {
    when ((index != 0) || (loop$index != 0)) {
      data[index] = loop$index;
      when (repeated == loop$index) count -= 1;
$inner
      when (repeated == loop$index) count += 1;
    }
  }
}
END
    $body =~ s/^/  /gm;
    return $body;
}

print run(0);
