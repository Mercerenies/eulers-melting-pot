
method Main() {
  var sieve := new bool[1000004];
  var primes := new int[1000004];
  var primes_pos := 0;

  var i := 0;
  var j := 0;

  while i < 1000004 {
    sieve[i] := true;
    i := i + 1;
  }

  sieve[0] := false;
  sieve[1] := false;
  i := 2;
  while i < 1000004 invariant primes_pos < 1000004 {
    if sieve[i] {
      primes[primes_pos] := i;
      if primes_pos < 1000003 {
        primes_pos := primes_pos + 1;
      }
      j := i + i;
      while (j < 1000004) {
        sieve[j] := false;
        j := j + i;
      }
      assert primes_pos < 1000004;
    }
    i := i + 1;
  }

  var sum := 0;
  var idx := 2;
  while idx < primes_pos - 1 {

    var p1 := primes[idx];
    var p2 := primes[idx + 1];

    var m := 1;
    while m < p1 {
        m := m * 10;
    }

    var old_r := p2;
    var r := m;
    var old_s := 1;
    var s := 0;
    var old_t := 0;
    var t := 1;
    while r != 0 {
      var quotient := old_r / r;
      var tmp := 0;

      tmp := old_r;
      old_r := r;
      r := tmp - quotient * r;

      tmp := old_s;
      old_s := s;
      s := tmp - quotient * s;

      tmp := old_t;
      old_t := t;
      t := tmp - quotient * t;

    }

    var inv := (old_s % m + m) % m;
    var smallest := ((p1 * inv) % m) * p2;

    sum := sum + smallest;

    idx := idx + 1;
  }

  print sum;
  print "\n";
}
