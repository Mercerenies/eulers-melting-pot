@ECHO OFF

SETLOCAL enabledelayedexpansion

SET d0=1000
SET d1=1000

SET max=0

:outer
IF !d0! GTR 100 (
   SET /a d0=d0-1
   SET /a d1=d0+1
   :inner
   IF !d1! GTR 100 (
      SET /a d1=d1-1
      SET /a temp=d0*d1

      IF !max! LSS !temp! (
         REM Exploiting the fact that the product will never be greater than six digits,
         REM a more efficient palindrome algorithm can be used
         IF "!temp:~0,1!"=="!temp:~-1,1!" (
            IF "!temp:~1,1!"=="!temp:~-2,1!" (
               IF "!temp:~2,1!"=="!temp:~-3,1!" (
                  SET max=!temp!
               )
            )
         )
      )

      GOTO inner
   )
   GOTO outer
)
ECHO %max%
