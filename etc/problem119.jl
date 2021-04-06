
# Too slow, as I figured. But this does find 2 and 10 correctly, which is nice.

function ispowerof(n, i)
    if i == 1
        return n == 1
    end
    x = i
    while n > x
        x = x * i
    end
    x == n
end

function digits(n)
    if n < 10
        [n]
    else
        result = digits(n ÷ 10)
        push!(result, n % 10)
        result
    end
end

digsum(n) = sum(digits(n))

isvalid(n) = ispowerof(n, digsum(n))

function findnth(target)
    idx = 0
    n = BigInt(10)
    while true
        if n % 1000000 == 0
            println(n)
        end
        if isvalid(n)
            idx = idx + 1
            if idx == target
                return n
            end
        end
        n = n + 1
    end
end

println(findnth(30))
