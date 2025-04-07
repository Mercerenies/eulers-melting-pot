
# We take the sums one step at a time. Each partial sum is stored as a
# pair containing the largest number used in the sum so far, as well
# as a Boolean indicating whether or not we've found duplicates.
#
# Still wrong: I tried to do fancy stuff to deal with the
# over-poisoning but it didn't work.

struct PartialSum
    largestindex::Int
    nextlargestindex::Union{Nothing, Int}
    isunique::Bool
end

SumVector = Vector{Union{Nothing, PartialSum}}

largestindex(ps::PartialSum) = ps.largestindex

nextlargestindex(ps::PartialSum) = ps.nextlargestindex

isunique(ps::PartialSum) = ps.isunique
isunique(::Nothing) = false

function mergesum(::Nothing, new::PartialSum)
    return new
end
function mergesum(old::PartialSum, new::PartialSum)
    allindices = [old.largestindex, old.nextlargestindex, new.largestindex, new.nextlargestindex]
    allindices = filter((x) -> !isnothing(x), allindices)
    sort!(allindices)
    return PartialSum(allindices[2], allindices[1], false)
end

function uniquesums(addends::Int, maxindex::Int)
    sums::SumVector = fill(nothing, addends * maxindex^2 + 1)
    sums[1] = PartialSum(0, nothing, true) # Pre-initialize trivial sum of 0 from zero addends.
    for i = 1:addends
        println("=== LOOP $i")
        sums = iteratesums(sums, addends-i, maxindex)
    end
    #println("dups: ", findall((x) -> !isnothing(x) && !isunique(x), sums) .- 1)
    #println("solns: ", findall(isunique, sums) .- 1)
    return sum(findall(isunique, sums) .- 1)
end

function iteratesums(oldsums::SumVector, addends::Int, maxindex::Int)::SumVector
    newsums::SumVector = fill(nothing, length(oldsums))
    for i in 1:length(oldsums)
        partialsum = oldsums[i]
        if !isnothing(partialsum)
            a = largestindex(partialsum)+1
            if !isunique(partialsum) && !isnothing(nextlargestindex(partialsum))
                a = nextlargestindex(partialsum) + 1 # It's possible to "regain" uniqueness with small indices
            end
            for j in a:(maxindex-addends)
                if j <= largestindex(partialsum)
                    newsum = PartialSum(j, nothing, true)
                else
                    newsum = PartialSum(j, nothing, isunique(partialsum))
                end
                #if i + j^2 - 1 == 166
                #    println("***")
                #    println(i-1, " ", j, " ", partialsum)
                #    println(newsums[i + j^2])
                #    println(newsum)
                #end
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
    println("bruteforce: ", results)
    return sum(results)
end

#println(bruteforce())
println(uniquesums(50, 100))
