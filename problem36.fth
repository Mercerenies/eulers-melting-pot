
: REVERSENHELPER ( x h n -- x1 )
  ROT
  DUP
  0 = IF
    DROP DROP
  ELSE
    OVER OVER SWAP /
    SWAP 2 PICK MOD
    3 ROLL 3 PICK * +
    ROT RECURSE
  THEN ;

: REVERSEN ( x n -- x1 )
  0 SWAP REVERSENHELPER ;

: ?COUNTS ( x -- x1 | Returns the number if it counts, or zero otherwise )
  DUP DUP
  2 REVERSEN = IF
    DUP DUP
    10 REVERSEN = IF
    ELSE
      DROP 0
    THEN
  ELSE
    DROP 0
  THEN ;

: PROBLEM ( -- )
  0
  1000000 1 ?DO
    I ?COUNTS +
    2
  +LOOP
  . ;

PROBLEM
