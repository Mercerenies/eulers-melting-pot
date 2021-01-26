#!/usr/bin/perl

use strict;
use warnings;
use 5.010;

my @ingr = (
    'cocoa powder',
    'eggs',
    'baking soda',
    'chocolate chips',
    'raisins',
    'oatmeal',
    'macadamia nuts',
    'peanut butter',
    'vanilla ice cream',
    'chocolate bars',
    'pie crusts',
    'chocolate sauce',
    'sugar',
    'chocolate ice cream',
    'flour',
    'lemon powder',
    'powdered sugar',
    'strawberry ice cream',
    'yeast',
    'caramel sauce',
    'vanilla extract',
    'salt',
);

print <<'PROLOGUE';

Non-Bouncy Numbers with Chocolate Sauce.

Ingredients.
0 olives
101 teaspoons baking powder
1 g cocoa powder
3628800 eggs
11 teaspoons baking soda
725760 chocolate chips
41 raisins
120960 g oatmeal
71 macadamia nuts
17280 g peanut butter
5173 teaspoons vanilla ice cream
172800 chocolate bars
4703 pie crusts
34560 g chocolate sauce
34913 g sugar
90720 teaspoons chocolate ice cream
117697 g flour
181440 g lemon powder
4913 g powdered sugar
8400 teaspoons strawberry ice cream
71 g yeast
90 g caramel sauce
1 cup vanilla extract
1 g salt
902 teaspoons milk

Method.
Put olives into 1st mixing bowl.
PROLOGUE

for (0..10) {
    my $n = $_ * 2;
    my $d = $n + 1;
    print "Combine baking powder into 1st mixing bowl. Put $ingr[$n] into 2nd mixing bowl. Divide $ingr[$d] into 2nd mixing bowl. Fold $ingr[$n] into 2nd mixing bowl. Add $ingr[$n] to 1st mixing bowl. "
}

print <<'EPILOGUE';
Remove milk from 1st mixing bowl. Pour contents of the 1st mixing bowl into the 1st baking dish.

Serves 1.
EPILOGUE

# Remember to change the 71 / 90 coefficient to be subtraction :)
