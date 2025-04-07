
# We take the sums one step at a time. Each partial sum is stored as a
# pair containing the largest number used in the sum so far, as well
# as a Boolean indicating whether or not we've found duplicates.
#
# Undercounts: We lose solutions when we count something as non-unique
# and then a unique sum is formed of it (essentially we're poisoning
# too many values)

struct PartialSum
    largestindex::Int
    isunique::Bool
end

SumVector = Vector{Union{Nothing, PartialSum}}

largestindex(ps::PartialSum) = ps.largestindex

isunique(ps::PartialSum) = ps.isunique
isunique(::Nothing) = false

mergesum(::Nothing, new::PartialSum) = new
mergesum(old::PartialSum, new::PartialSum) = PartialSum(min(old.largestindex, new.largestindex), false)

function uniquesums(addends::Int, maxindex::Int)
    sums::SumVector = fill(nothing, addends * maxindex^2 + 1)
    sums[1] = PartialSum(0, true) # Pre-initialize trivial sum of 0 from zero addends.
    for i = 1:addends
        println("=== LOOP $i")
        sums = iteratesums(sums, addends-i, maxindex)
    end
    println(findall((x) -> !isnothing(x) && !isunique(x), sums) .- 1)
    println(findall(isunique, sums) .- 1)
    return sum(findall(isunique, sums) .- 1)
end

function iteratesums(oldsums::SumVector, addends::Int, maxindex::Int)::SumVector
    newsums::SumVector = fill(nothing, length(oldsums))
    for i in 1:length(oldsums)
        partialsum = oldsums[i]
        if !isnothing(partialsum)
            for j in (largestindex(partialsum)+1):(maxindex-addends)
                newsum = PartialSum(j, isunique(partialsum))
                if i + j^2 == 86
                    println(i-1, " ", j, " ", partialsum)
                    println(newsum, " ", newsums[i + j^2])
                end
                newsums[i + j^2] = mergesum(newsums[i + j^2], newsum)
            end
        end
    end
    return newsums
end

# Brute-force direct iteration for 3 elems
function bruteforce()
    counts = Dict{Int, Int}()
    for i in 1:10
        for j in (i+1):10
            for k in (j+1):10
                counts[i^2 + j^2 + k^2] = get(counts, i^2 + j^2 + k^2, 0) + 1
            end
        end
    end
    results = []
    for (k, v) in counts
        if v == 1
            push!(results, k)
        end
    end
    sort!(results)
    println(results)
    return sum(results)
end

println(bruteforce())
println(uniquesums(3, 10))
