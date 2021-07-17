
import Primes

# The remainder when n is divided into A(x)
function reprem(n, x)
    cache = Dict()
    cycledata = []
    remainder = 0
    for i in 1:x
        if remainder in keys(cache)
            match = cache[remainder]
            cycle = cycledata[match:end]
            remaining = x - i + 2
            return cycle[(remaining-1) % length(cycle) + 1]
        end
        push!(cache, remainder => i)
        push!(cycledata, remainder)
        remainder = (remainder * 10 + 1) % n
    end
    remainder
end

function run()
    sum = 0
    remaining = 40
    for i in Iterators.countfrom(2)
        if Primes.isprime(i) && reprem(i, 10 ^ 9) == 0
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
