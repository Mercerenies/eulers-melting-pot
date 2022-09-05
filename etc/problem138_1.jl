
# From the numbers produced in problem138_1.jl, it's OEIS A007805
#
# We'll use the recurrence a(n) = 18*a(n-1) - a(n-2), with a(0)=1,
# a(1)=17

using Printf

function run()
    a = 1
    b = 17
    sum = 0
    for i in 1:12
        sum += b
        a, b = b, 18 * b - a
    end
    sum
end

@printf "%d\n" run()
