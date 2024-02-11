
# Trying some linear algebra to get rid of the caching dictionary in
# Problem 178.
#
# Reference: https://www.jeanmariedekoninck.mat.ulaval.ca/fileadmin/Documents/Publications/2009_esthetic_numbers.pdf
#
# Fix q = 10 (the radix we're working in, which for this problem is
# always base 10). Write N(r) to be the total number of step numbers
# of length exactly r. Write N(r, i) to be the number of step numbers
# of length exactly r which end in i. (Assume r > 0 and 0 <= i < q)
#
# We have the base cases
#
# (*) N(0, 0) = 0
# (*) N(0, i) = 1 for i != 0
#
# and the recurrences
#
# (*) N(r, 0) = N(r - 1, 1)
# (*) N(r, q - 1) = N(r - 1, q - 2)
# (*) N(r, i) = N(r - 1, i - 1) + N(r - 1, i + 1) for 1 <= i <= q - 2
#
# Define the q*q matrix M by
#
# M_{i,j} = 1 if |i-j| = 1
# M_{i,j} = 0 otherwise
#
# and the vector u as u = (0, 1, 1, ..., 1) of length q.
#
# Then note that N(r, i) = (M^(r-1) u)_(i+1)
#
# (Note: Vector indexing is one-based, hence the "+1" in the subscript)
#
# And N(r) = sum(i=0 to q-1, N(r, i))
#
# Next, as we did in problem178.lua, we want to subtract the cases
# that don't have zeroes and those that don't have nines, then add
# back in the ones that have neither.
#
# The ones that don't have nines are easy. We just do the exact same
# computation with q = 9 instead of q = 10. The zeroes are a bit
# different, since leading zeroes are already forbidden. To calculate
# with the missing zeroes, we use u = (1, 1, ..., 1) and q = 9. Then,
# with both constraints in place, we do u = (1, 1, ..., 1) and q = 8.


function n(q, r; leadingzeros=false)
    m = zeros(Int, q, q)
    for i in 2:q
        m[i, i-1] = 1
        m[i-1, i] = 1
    end
    u = ones(Int, q)
    if !leadingzeros
        u[1] = 0
    end
    sum(m ^ (r - 1) * u)
end

let total = 0
    for r = 10:40
        total += n(10, r) - n(9, r) - n(9, r, leadingzeros=true) + n(8, r, leadingzeros=true)
    end
    println(total)
end
