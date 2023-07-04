
# Mostly done Miller-Rabin in Julia, might be useful in future
# challenges.

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

function lasttwo(n::Integer)
    while n % 10 == 0
        n ÷= 10
    end
    return n % 100
end

function run()
    a = BigInt(1)
    for i in 1:1000
        a *= i
        println("$i factorial = $(lasttwo(a))")
    end
end

run()
