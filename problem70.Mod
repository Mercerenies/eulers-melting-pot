
MODULE problem70;
IMPORT Out;
VAR
     primes: ARRAY 10000001 OF BOOLEAN;
     i: INTEGER;
     j: INTEGER;
     min: INTEGER;
     minratio: REAL;
     value: INTEGER;
     ratio: REAL;

(* Note: This Phi function suffers from the same issue as
 * problem69.m, in that it systematically produces an erroneously
 * large value for some inputs. In this case, we're trying to
 * minimize n / Phi(n), thus we want a large enough Phi(n). By
 * a similar argument to that of problem69.m, we know that the
 * desired result should be a product of a small number of distinct
 * primes (a single prime will never be permutable to its phi value).
 * By direct inspection, there exist pairs of primes satisfying this
 * constraint, hence the solution will be one of them, and this Phi
 * function will not overcount far enough to corrupt the solution.
 *)
PROCEDURE Phi(n: INTEGER): INTEGER;
VAR
     result: INTEGER;
     prod: REAL;
     p: INTEGER;
BEGIN
     IF primes[n] THEN
          result := n - 1;
     ELSE
          p := 2;
          prod := FLT(n);
          WHILE p * p <= n DO
               IF primes[p] & (n MOD p = 0) THEN
                    prod := prod * (1.0 - 1.0 / FLT(p));
                    IF (p * p # n) & primes[n DIV p] THEN
                         prod := prod * (1.0 - FLT(p) / FLT(n));
                    END;
               END;
               p := p + 1;
          END;
          result := FLOOR(prod);
     END;
     RETURN result
END Phi;

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
     primes[0] := FALSE;
     primes[1] := FALSE;
     FOR i := 2 TO 10000000 DO
          primes[i] := TRUE;
     END;
     FOR i := 2 TO 10000000 DO
          IF primes[i] THEN
               j := i + i;
               WHILE j <= 10000000 DO
                    primes[j] := FALSE;
                    j := j + i;
               END;
          END;
     END;
     min := 0;
     minratio := 99999999.0;
     FOR j := 2 TO 10000000 DO
          value := Phi(j);
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
