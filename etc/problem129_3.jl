
function opta(n)
    product = 1
    i = 2
    while n != 1
        count = 0
        while n % i == 0
            count += 1
            n = div(n, i)
        end
        if count > 0
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
            product = lcm(product, x * i ^ (count - 1))
        end
        i += 1
    end
    product
end

# Since a(n) <= n, we can start our search at n = 1000000
lastdigit = [1 3 7 9]
for n = Iterators.countfrom(1000000, 10), d = lastdigit
    if opta(n + d) > 1000000
        println(n + d)
        break
    end
end

#=

/                                       \
@(1000000 (ovs's[)84+84 01%01(v):v `w]v)/
            \c=c(=)           /  \=(=)/


=#
