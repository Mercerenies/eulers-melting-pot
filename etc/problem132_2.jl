
import Primes

# A number, stored as a function from (1-indexed) positions in the
# number to the base 10 digit at that position.
abstract type DigitNumber end

# 1111...11111
struct Repunit <: DigitNumber
    length::Int
end

digit(::Repunit, n) = 1
cyclelength(::Repunit) = 1
totallength(value::Repunit) = value.length

struct CyclicNumber <: DigitNumber
    cycle::Vector{Int}
    cycles::Int
end

digit(value::CyclicNumber, n) = value.cycle[(n - 1) % length(value.cycle) + 1]
cyclelength(value::CyclicNumber) = length(value.cycle)
totallength(value::CyclicNumber) = length(value.cycle) * value.cycles

struct OrdinaryNumber <: DigitNumber
    number::Int
end

digit(value::OrdinaryNumber, n) = div(value.number, 10 ^ (n - 1)) % 10
cyclelength(value::OrdinaryNumber) = length(string(value.number))
totallength(value::OrdinaryNumber) = length(string(value.number))

# Remainder when dividing the number n into the DigitNumber value.
function divrem(n, value::DigitNumber)
    cache = Dict()
    cycledata = []
    remainder = 0
    for i in totallength(value):-1:1
        j = i % cyclelength(value)
        if (remainder, j) in keys(cache)
            match = cache[(remainder, j)]
            cycle = cycledata[match:end]
            remaining = i + 2
            return cycle[(remaining-1) % length(cycle) + 1]
        end
        push!(cache, (remainder, j) => i)
        push!(cycledata, remainder)
        remainder = (remainder * 10 + digit(value, i)) % n
    end
    remainder
end

println(divrem(106, Repunit(11)))
