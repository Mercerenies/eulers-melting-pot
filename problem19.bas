YEAR% = 1900
MONTH% = 1
DAY% = 7 ' The first Sunday
TOTAL% = 0
WHILE YEAR% < 2001
    DAY% = DAY% + 7
    SELECT CASE MONTH%
        CASE 4, 6, 9, 11
            DAYS% = 30
        CASE 1, 3, 5, 7, 8, 10, 12
            DAYS% = 31
        CASE 2
            IF YEAR% MOD 4 = 0 AND (YEAR% MOD 100 <> 0 OR YEAR% MOD 400 = 0) THEN
                DAYS% = 29
            ELSE
                DAYS% = 28
            END IF
    END SELECT
    IF DAY% > DAYS% THEN
        MONTH% = MONTH% + 1
        DAY% = DAY% MOD DAYS%
        IF YEAR% > 1900 AND DAY% = 1 THEN
            TOTAL% = TOTAL% + 1
        END IF
        IF MONTH% > 12 THEN
            MONTH% = MONTH% MOD 12
            YEAR% = YEAR% + 1
        END IF
    END IF
WEND
PRINT TOTAL%
