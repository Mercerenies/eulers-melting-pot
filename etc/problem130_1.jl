
function opta(n)
    n0 = n
    product = 1
    i = 2
    isprime = true
    while n != 1
        count = 0
        while n % i == 0
            if n != i
                isprime = false
            end
            count += 1
            n = div(n, i)
        end
        if count == 0
            i0 = i
            i = 3
        end
        if i == 3
            x = 3
        else
            acc = 10 % i
            x = 1
            while acc != 1
                acc = (acc * 10) % i
                x += 1
            end
        end
        if count > 0
            product = lcm(product, x * i ^ (count - 1))
        else
            i = i0
        end
        i += 1
    end
    # If it's prime, just return the number, so that it definitely
    # won't divide into N - 1 evenly.
    if isprime
        n0
    else
        product
    end
end

function solve(remaining)
    sum = 0

    lastdigit = [1 3 7 9]
    for n = Iterators.countfrom(10, 10), d = lastdigit
        i = n + d
        a = opta(i)
        if (i - 1) % a == 0
            remaining -= 1
            sum += i
            if remaining == 0
                break
            end
        end
    end

    sum
end

println(solve(25))
