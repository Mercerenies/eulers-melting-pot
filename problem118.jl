
PRIME_UPPER = 99999999
SIEVE = [true for _ in 1:PRIME_UPPER]

function isprimetest(x)
    if x < 2
        false
    else
        for i = 2 : floor(Int64, x / 2)
            if x % i == 0
                return false
            end
        end
        true
    end
end

function isprime(x)
    if x < 2
        false
    elseif x < PRIME_UPPER
        SIEVE[x]
    else
        isprimetest(x)
    end
end

tonumber(lst) = reduce((acc, x) -> 10 * acc + x, lst, init=0)

# Heap's algorithm (https://en.wikipedia.org/wiki/Heap%27s_algorithm)
function _permutations(arr, k, results)
    if k == 1
        push!(results, copy(arr))
    else
        _permutations(arr, k - 1, results)
        for i = 1:k-1
            if k % 2 == 0
                arr[[i,k]] = arr[[k,i]]
            else
                arr[[1,k]] = arr[[k,1]]
            end
            _permutations(arr, k - 1, results)
        end
    end
    nothing
end

function permutations(arr)
    results = []
    _permutations(arr, length(arr), results)
    results
end

function countsolutions(lst)
    if length(lst) > 1 && sum(lst) % 3 == 0
        return 0 # All permutations will be divisible by 3
    end
    count(x -> isprime(tonumber(x)), permutations(lst))
end

function _subsets(lst, idx)
    if idx > length(lst)
        [[]]
    else
        rest = _subsets(lst, idx + 1)
        [rest ; map(xs -> [lst[idx] ; xs], rest)]
    end
end

subsets(lst) = _subsets(lst, 1)

function partitions(lst)
    if isempty(lst)
        [[]]
    else
        results = []
        for sub = subsets(lst[2:end])
            push!(sub, lst[1])
            remaining = sort(collect(setdiff(Set(lst), Set(sub))))
            for xs = partitions(remaining)
                push!(results, [[sub];xs])
            end
        end
        results
    end
end

function run(digits)
    count = 0
    for part = partitions(collect(1:digits))
        inner = 1
        for xs = part
            tmp = countsolutions(xs)
            if tmp == 0
                inner = 0
                break
            end
            inner = inner * tmp
        end
        count = count + inner
    end
    count
end

# Run the sieve
SIEVE[1] = false
for i = 1:PRIME_UPPER
    if SIEVE[i]
        j = i + i
        while j < PRIME_UPPER
            SIEVE[j] = false
            j = j + i
        end
    end
end

println(run(9))
