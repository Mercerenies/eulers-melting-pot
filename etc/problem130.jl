
# As usual, let's start with naive brute-force, which I know won't
# work. But we should try it anyway.
#
# Aaaaaand that, kids, is why we try brute-forcing it first. It
# actually works.

import Primes

struct PrimeFactor{T}
    factor::T
    count::Int64
end

function primefactors(n)
    result = []
    i = 2
    while n != 1
        count = 0
        while n % i == 0
            count += 1
            n = div(n, i)
        end
        if count > 0
            push!(result, PrimeFactor(i, count))
        end
        i += 1
    end
    result
end

function primea(p)

    # 3 is a silly corner case so it gets to sit in the silly corner :)
    if p == 3
        return 3
    end

    acc = 10 % p
    x = 1
    while acc != 1
        acc = (acc * 10) % p
        x += 1
    end
    x
end

function opta(n)
    factors = primefactors(n)
    reduce(lcm, map((x) -> primea(x.factor) * x.factor ^ (x.count - 1), factors))
end

function solve(remaining)
    sum = 0

    lastdigit = [1 3 7 9]
    for n = Iterators.countfrom(10, 10), d = lastdigit
        i = n + d
        a = opta(i)
        if (i - 1) % a == 0 && !Primes.isprime(i)
            remaining -= 1
            sum += i
            if remaining == 0
                break
            end
        end
    end

    sum
end

println(solve(25))

