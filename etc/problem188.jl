
# Naive attempt at Problem 188.

function tetration(a, b, m)
    if b == 1
        a
    else
        powermod(a, tetration(a, b - 1, m), m)
    end
end

println(tetration(1777, 1855, 100_000_000))
