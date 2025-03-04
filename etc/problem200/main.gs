# Failed attempt at 200 in Goboscript. The language is somewhat inconsistent, but
# moreover Scratch is just too slow to be an executor for this.

costumes "blank.svg";

func miller_rabin_test(n, d, r, a) {
    local x = pow_mod($a, $d, $n);
    if x == 1 or x == $n - 1 {
        return true;
    }
    repeat $r {
        x = (x * x) % $n;
        if x == $n - 1 {
            return true;
        }
    }
    return false;
}

func pow_mod(a, b, n) {
    if $b == 0 {
        return 1;
    } elif $b % 2 == 0 {
        local x = pow_mod($a, $b / 2, $n);
        return (x * x) % $n;
    } else {
        # Any calls need to be immediately assigned to a variable
        # or the compiler does weird things.
        local x = pow_mod($a, $b - 1, $n);
        return (x * $a) % $n;
    }
}

func is_prime(n) {
    if $n < 2 {
        return false;
    }
    if $n == 2 or $n == 3 or $n == 5 or $n == 13 or $n == 23 or $n == 1662803 {
        return true;
    }
    if $n % 2 == 0 or $n % 3 == 0 or $n % 5 == 0 {
        return false;
    }
    local t = 0;
    if $n >= 1122004669633 {
        t = is_prime_naive($n);
        return t;
    }

    local d = $n - 1;
    local r = 0;
    until d % 2 == 1 {
        d = d / 2;
        r = r + 1;
    }
    t = miller_rabin_test($n, d, r, 2);
    if not t {
        return false;
    }
    t = miller_rabin_test($n, d, r, 13);
    if not t {
        return false;
    }
    t = miller_rabin_test($n, d, r, 23);
    if not t {
        return false;
    }
    t = miller_rabin_test($n, d, r, 1662803);
    if not t {
        return false;
    }
    return true;
}

func is_prime_naive(n) {
    local i = 2;
    until i * i > $n {
        if $n % i == 0 {
            return false;
        }
        i = i + 1;
    }
    return true;
}

func next_prime(n) {
    local i = $n;
    local t = is_prime(i);
    until t {
        i = i + 1;
        t = is_prime(i);
    }
    return i;
}

onflag {
    say next_prime(12783);
}
