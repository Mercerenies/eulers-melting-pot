[999999]BOOL sieve;
[999999]INT primes;
INT prime count := 0;
INT current length := 0;
INT current index := 0;
INT total sum := 0;
INT current sum := 0;

# Sieve of Eratosthenes #
sieve[1] := FALSE;
FOR i FROM 2 TO 999999 DO
  sieve[i] := TRUE
OD;
FOR i FROM 2 TO 999999 DO
  IF sieve[i] THEN
    FOR j FROM i + i BY i TO 999999 DO
      sieve[j] := FALSE
    OD
  FI
OD;

# Get the list of primes #
FOR i FROM 2 TO 999999 DO
  IF sieve[i] THEN
    prime count +:= 1;
    primes[prime count] := i
  FI
OD;
FOR i FROM prime count + 1 TO 999999 DO
  primes[i] := -1
OD;

# Sum up the primes #
FOR i FROM 1 TO prime count DO
  IF total sum < 999999 THEN
    total sum +:= primes[i]
  ELSE
    prime count -:= 1
  FI
OD;

# Do the algorithm #
current length := prime count;
WHILE current length > 0 DO
  current sum := total sum;
  IF current sum <= 999999 THEN
    current index := 1;
    WHILE current index + current length <= prime count DO
      IF current sum <= 999999 AND current sum > 0 THEN
        IF sieve[current sum] THEN
          print(current sum);
          print(new line);
          stop
        FI
      FI;
      current sum -:= primes[current index];
      current sum +:= primes[current index + current length];
      current index +:= 1
    OD
  FI;
  total sum -:= primes[current length];
  current length -:= 1
OD
