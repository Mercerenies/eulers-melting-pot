
# Let's try a clever thing with a priority queue

using DataStructures

KNOWN_SOLUTIONS = Set()

abstract type SearchNode end

# Start a new search index
struct NextIndex <: SearchNode
    index::BigInt
end

nodepriority(node::NextIndex) = node.index

function runnode(pq, node::NextIndex)
    addtoqueue!(pq, NextIndex(node.index + 1))
    addtoqueue!(pq, Branch(node.index))
end

# Keep searching an existing branch
struct Branch <: SearchNode
    index::BigInt
    product::BigInt
end

Branch(index) = Branch(index, index)

nextbranch(branch::Branch) = Branch(branch.index, branch.product * branch.index)

nodepriority(node::Branch) = node.product

function runnode(pq, node::Branch)
    if node.product >= 10 && digitsum(node.product) == node.index
        push!(KNOWN_SOLUTIONS, node.product)
    end
    addtoqueue!(pq, nextbranch(node))
end

function runonestep!(pq)
    curr = dequeue!(pq)
    runnode(pq, curr)
end

function startingqueue()
    queue = PriorityQueue{SearchNode, BigInt}()
    addtoqueue!(queue, NextIndex(2))
    queue
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

digitsum(n) = sum(digits(n))

addtoqueue!(pq, k::SearchNode) = enqueue!(pq, k, nodepriority(k))

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

function confidentresults(pq)
    # Returns only the prefix of the KNOWN_SOLUTIONS that we are 100%
    # certain is correct and complete.
    front = peek(pq).second
    known = takewhile(x -> x < front, sort(collect(KNOWN_SOLUTIONS)))
end

function confidentlyget(pq, n)
    results = confidentresults(pq)
    if length(results) < n
        nothing
    else
        results[n]
    end
end

queue = startingqueue()
while true
    runonestep!(queue)
    result = confidentlyget(queue, 10)
    if result != nothing
        #println(result)
        println(confidentresults(queue))
        break
    end
end
