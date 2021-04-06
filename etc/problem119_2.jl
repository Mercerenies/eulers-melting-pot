
KNOWN_SOLUTIONS = Set()

function digits(n)
    if n < 10
        [n]
    else
        result = digits(n ÷ 10)
        push!(result, n % 10)
        result
    end
end

digitsum(n) = sum(digits(n))

biggestdigsum(n) = 9 * length(digits(n))

function findall(power)
    base = BigInt(1)
    curr = 1
    while biggestdigsum(curr) >= base
        if digitsum(curr) == base
            push!(KNOWN_SOLUTIONS, curr)
        end
        base = base + 1
        curr = base ^ power
    end
end

function takewhile(pred, iter)
    # This is in Julia 1.4, but I'm running an old Julia so I'll
    # define it here. This version just returns an array, because
    # that's all I need it to do.
    arr = []
    for x = iter
        if !pred(x)
            break
        end
        push!(arr, x)
    end
    arr
end

function confidentresults(power)
    # Returns only the prefix of KNOWN_SOLUTIONS that we are 100%
    # certain is correct and complete.
    smallest = BigInt(2) ^ power
    takewhile(x -> x < smallest, sort(collect(KNOWN_SOLUTIONS)))
end

function confidentlyget(power, n)
    results = confidentresults(power)
    if length(results) < n
        nothing
    else
        results[n]
    end
end

function run()
    power = 2
    while true
        findall(power)
        # NOTE: Index 31, because our algorithm here includes 1 as a
        # solution for technical reasons, which we want to ignore.
        result = confidentlyget(power, 31)
        if result != nothing
            println(result)
            break
        end
        power = power + 1
    end
end

run()
