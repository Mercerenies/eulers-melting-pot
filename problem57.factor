
USING: kernel math math.parser sequences prettyprint ;
IN: main

: gcd ( n m -- n! ) [ dup 0 > ] [ dup -rot mod ] while drop ;

: reduce ( num den -- num! den! ) 2dup gcd dup [ / ] dip swapd / swap ;

: iterate ( num den -- num! den! ) over 2 * + swap ;

: check ( num den -- ? ) [ - ] keep reduce [ number>string length ] bi@ > ;

0 2 1 1000

[ 1 - dup 0 > ] [ [ iterate 2dup check [ [ 1 + ] 2dip ] [ ] if ] dip ] while

3drop .
