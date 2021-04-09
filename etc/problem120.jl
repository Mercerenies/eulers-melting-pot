
f(a, n) = (powermod(a - 1, n, a ^ 2), powermod(a + 1, n, a ^ 2))

function run(a)
    visited = Set()
    rmax = 0
    n = 1
    while f(a, n) ∉ visited
        push!(visited, f(a, n))
        rmax = max(rmax, +(f(a, n)...) % (a ^ 2))
        n += 1
    end
    rmax
end

println(sum(run, 3:1000))

# Variables
#  1 - Cumulative sum
#  2 - a
#  3 - visited (array)
#  4 - rmax
#  5 - n
#  6 - powermod ( a b n -- power ) (CLOBBERS 7 AND 8)
#  7 - tmp0
#  8 - tmp1
#  9 - tmp2
# 10 - tmp3
# 11 - tmp4
# 12 - outer_a
# 13 - ???
# 14 - f ( a n -- x y ) (CLOBBERS 7, 8, 9, 10)
# 15 - member? (x arr -- ? ) (CLOBBERS 7, 8)
# 16 - index
# 17 - run ( a -- rmax ) (CLOBBERS EVERYTHING)
# 18 - max ( a b -- a/b ) (CLOBBERS 7, 8)

# Encoding scheme: multiply (a-1)^n * a^2 + (a+1)^n
# Encoding scheme: multiply y * a^2 + x

# Top of Stack
# xy -> 11

# Top of stack
# a>b
# b -> 7
# a -> 8
