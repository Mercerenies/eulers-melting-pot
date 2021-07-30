
IsPrime(n) {
    if (n < 2) {
        return false
    } else {
        x = 2
        while (x <= Sqrt(n)) {
            if (Mod(n, x) == 0) {
                return false
            }
            x += 1
        }
        return true
    }
}

PowerMod(a, b, n) {
    p = 2
    x = 1
    t := b

    while (p * 2 <= b) {
        p *= 2
    }

    while (p > 0) {
        if (t >= p) {
            x := Mod(x * a, n)
            t -= p
        }
        p //= 2
        if (p > 0) {
            x := Mod(x * x, n)
        }
    }
    return x
}

IsEventuallyOne(p) {
    number := Mod(10, p)
    while (not visited%number%) {
        visited%number% = 1
        number := PowerMod(number, 10, p)
    }
    return number == 1
}

WillDivideRep(p) {
    return IsEventuallyOne(9 * p)
}

sum = 0
p = 2
while (p < 100000) {
    if (IsPrime(p) and not WillDivideRep(p)) {
        sum += p
    }
    p += 1
}
MsgBox % sum
