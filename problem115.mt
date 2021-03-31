
ARR VAR gray .
ARR VAR red .

1 VAR n .

1 VAR continue .
WHILE continue
    gray WIPE red WIPE . .

    gray 1 APPEND . .
    red 1 APPEND . .

    1 VAR i .
    1 VAR icontinue .
    WHILE icontinue

        1 VAR g .
        0 VAR j .
        i 49 - j - VAR jcontinue .
        WHILE jcontinue
            red GET j g + VAR g . .
            j 1 + VAR j .
            i 49 - j - VAR jcontinue .
        ENDWHILE
        gray g APPEND . .

        1 VAR r .
        0 VAR j .
        i j - VAR jcontinue .
        WHILE jcontinue
            gray GET j r + VAR r . .
            j 1 + VAR j .
            i j - VAR jcontinue .
        ENDWHILE
        red r APPEND . .

        i 1 + VAR i .
        n 1 + i - VAR icontinue .
    ENDWHILE
    1000000 gray GET n SWAP . - VAR continue .

    n 1 + VAR n .
ENDWHILE
n 1 - PRINT .