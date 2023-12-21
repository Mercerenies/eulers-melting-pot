
USING: kernel math math.parser math.functions sequences prettyprint ;
IN: main

! Straight brute force, no tricks. n is the width of the lamina and m
! is the width of the hole.

CONSTANT: limit 1000000

: tiles-needed ( n m -- tiles )
    [ 2 ^ ] bi@ - ;

: valid-lamina? ( n m -- ? )
    [ nip 0 > ] [ tiles-needed limit <= ] 2bi and ;

: count-for-width ( n -- count )
    dup 2 - [ 2dup valid-lamina? ] [ 2 - ] while
    2 + - 2 / ;

limit 4 / 1 - <iota> [ 3 + count-for-width ] map sum .
