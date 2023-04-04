
# Miller-Rabin primality for the checks.
#
# According to Wikipedia, Miller-Rabin for the first nine primes is
# good up to well above (150 million)^2.

a_values = [2 3 5 7 11 13 17 19 23]
small_primes = [2 3 5 7 11 13 17 19 23 29 31 37 41 43]

function millerrabin(n)
    # Factor out powers of 2.
    d = n - 1
    s = 0
    while d % 2 == 0
        d ÷= 2
        s += 1
    end
    # Now do Miller-Rabin test for each 'a' value.
    for a in a_values
        x = powermod(a, d, n)
        y = convert(typeof(n), 1)
        for j in 1:s
            y = (x * x) % n
            if y == 1 && x != 1 && x != n - 1
                return false
            end
            x = y
        end
        if y != 1
            return false
        end
    end
    true
end

function isprime(n)
    # Check small primes by hand.
    for p in small_primes
        if n == p
            return true
        end
        if n % p == 0
            return false
        end
    end
    # Otherwise, do Miller-Rabin
    millerrabin(n)
end

function isvalid(n)
    n2 = n * n
    if !isprime(n2 + 1)
        return false
    end
    if isprime(n2 + 2)
        return false
    end
    if !isprime(n2 + 3)
        return false
    end
    if isprime(n2 + 4)
        return false
    end
    if isprime(n2 + 5)
        return false
    end
    if isprime(n2 + 6)
        return false
    end
    if !isprime(n2 + 7)
        return false
    end
    if isprime(n2 + 8)
        return false
    end
    if !isprime(n2 + 9)
        return false
    end
    if isprime(n2 + 10)
        return false
    end
    if isprime(n2 + 11)
        return false
    end
    if isprime(n2 + 12)
        return false
    end
    if !isprime(n2 + 13)
        return false
    end
    if isprime(n2 + 14)
        return false
    end
    if isprime(n2 + 15)
        return false
    end
    if isprime(n2 + 16)
        return false
    end
    if isprime(n2 + 17)
        return false
    end
    if isprime(n2 + 18)
        return false
    end
    if isprime(n2 + 19)
        return false
    end
    if isprime(n2 + 20)
        return false
    end
    if isprime(n2 + 21)
        return false
    end
    if isprime(n2 + 22)
        return false
    end
    if isprime(n2 + 23)
        return false
    end
    if isprime(n2 + 24)
        return false
    end
    if isprime(n2 + 25)
        return false
    end
    if isprime(n2 + 26)
        return false
    end
    if !isprime(n2 + 27)
        return false
    end
    return true
end

function run()
    sum = BigInt(0)
    for n in 10:10:149999999 # See results of 146_analysis.hs for why we can use a step size of 10.
        # And for why we can do all this modular chicanery
        if n % 3 == 0 || (n % 7 != 3 && n % 7 != 4) || (n % 13 != 1 && n % 13 != 3 && n % 13 != 4 && n % 13 != 9 && n % 13 != 10 && n % 13 != 12)
            continue
        end
        if isvalid(BigInt(n))
            sum += n
        end
    end
    println(sum)
end

run()
