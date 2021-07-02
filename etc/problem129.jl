
# Obviously, brute forcing this is a Very Bad Idea™. But I'm going to
# try it for small inputs just to get a better feel of the problem
# space.

r(k) = k == 1 ? 1 : 1 + 10 * r(k - 1)

function a(n)
    if gcd(n, 10) == 1
        for k in Iterators.countfrom(1)
            if r(k) % n == 0
                return k
            end
        end
    else
        error("Invalid input to a(n)")
    end
end

function solve(limit)
    for n in Iterators.countfrom(1)
        if gcd(n, 10) == 1 && a(n) > limit
            return n
        end
    end
end

println(solve(10)) # 17
