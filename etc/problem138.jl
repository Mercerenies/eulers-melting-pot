
using Printf

function run()
    count = 0
    sum = Int128(0)
    b = Int128(2)
    while true
        for h in [b - 1,b + 1]
            discr = h ^ 2 + div(b, 2) ^ 2
            side = isqrt(discr)
            if side * side == discr
                @printf "%d %d %d\n" b h side
                count += 1
                sum += side
                if count >= 12
                    return sum
                end
            end
        end
        b += 2
    end
end

@printf "%d\n" run()
