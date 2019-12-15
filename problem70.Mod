
MODULE problem70;
IMPORT Out;
VAR
     phi: ARRAY 10000001 OF REAL;
     i: INTEGER;
     j: INTEGER;
     min: INTEGER;
     minratio: REAL;
     value: INTEGER;
     ratio: REAL;

PROCEDURE IntToString(n: INTEGER; VAR arr: ARRAY OF CHAR; pos: INTEGER);
BEGIN
     IF n # 0 THEN
          IntToString(n DIV 10, arr, pos - 1);
          arr[pos] := CHR(n MOD 10 + ORD("0"));
     END;
END IntToString;

PROCEDURE Sort(VAR arr: ARRAY OF CHAR; size: INTEGER);
VAR
     k0: INTEGER;
     k1: INTEGER;
     minpos: INTEGER;
     tmp: CHAR;
BEGIN
     FOR k0 := 0 TO size - 1 DO
          minpos := k0;
          FOR k1 := k0 + 1 TO size - 1 DO
               IF arr[k1] < arr[minpos] THEN
                    minpos := k1;
               END;
          END;
          tmp := arr[minpos];
          arr[minpos] := arr[k0];
          arr[k0] := tmp;
     END;
END Sort;

PROCEDURE IsPerm(n: INTEGER; m: INTEGER): BOOLEAN;
VAR
     narr: ARRAY 8 OF CHAR;
     marr: ARRAY 8 OF CHAR;
     iter: INTEGER;
     final: BOOLEAN;
BEGIN
     FOR iter := 0 TO 7 DO
          narr[iter] := ".";
          marr[iter] := ".";
     END;
     IntToString(n, narr, 7);
     IntToString(m, marr, 7);
     Sort(narr, 8);
     Sort(marr, 8);
     final := TRUE;
     FOR iter := 0 TO 7 DO
          IF narr[iter] # marr[iter] THEN
               final := FALSE;
          END;
     END;
     RETURN final
END IsPerm;

BEGIN
     FOR i := 2 TO 10000000 DO
          phi[i] := FLT(i);
     END;
     FOR i := 2 TO 10000000 DO
          IF phi[i] = FLT(i) THEN
               j := i;
               WHILE j <= 10000000 DO
                    phi[j] := phi[j] * (1.0 - 1.0 / FLT(i));
                    j := j + i;
               END;
          END;
     END;
     min := 0;
     minratio := 99999999.0;
     FOR j := 2 TO 10000000 DO
          value := FLOOR(phi[j]);
          IF IsPerm(value, j) THEN
               ratio := FLT(j) / FLT(value);
               IF ratio < minratio THEN
                    min := j;
                    minratio := ratio;
               END;
          END;
     END;
     Out.Int(min, 8);
     Out.Ln();
END problem70.
