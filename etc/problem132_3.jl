
import Primes

# Whether p divides the repunit R(n)
function primedivides(n, p)
    powermod(10, n, 9 * p) == 1
end

function run()
    sum = 0
    remaining = 40
    for i in Iterators.countfrom(2)
        if primedivides(10 ^ 9, i) && Primes.isprime(i)
            sum += i
            remaining -= 1
            if remaining <= 0
                break
            end
        end
    end
    sum
end

println(run())
