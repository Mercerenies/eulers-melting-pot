
using Primes

let index = 1
    let current = 2
        while (powermod(current - 1, index, current ^ 2) + powermod(current + 1, index, current ^ 2)) % (current ^ 2) < 1e10
            index += 1
            current = Primes.nextprime(current + 1)
        end
        println(index)
    end
end
