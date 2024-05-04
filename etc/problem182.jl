
# Direct computation in Julia

# Too slow, by far, and I even made a mistake (not skipping over the
# correct number of e's when gcd is not 1)

function countunconcealed(e, n)
    count = 0
    for m in 0:n-1
        if powermod(m, e, n) == m
            count += 1
        end
    end
    count
end

let
    # Constants from the problem description
    p = 1009
    q = 3643
    n = p * q
    phi = (p - 1) * (q - 1)

    e_sum = 0
    best_min = n # Haven't found a min yet, so put it out of bounds
    m_values = 0:n-1
    acc_values = collect(0:n-1)

    for e in 2:phi-1
        if e % 1000 == 0
            println(e)
        end
        if gcd(e, phi) != 1
            continue
        end

        acc_values .*= m_values # Mistake here: We need to append multiple e's, not just one.
        acc_values .%= n
        unconcealed_count = sum(acc_values .== m_values)
        if unconcealed_count == best_min
            # Found another e value, so add it
            e_sum += e
        elseif unconcealed_count < best_min
            # Found a better match, everything so far is now invalid
            e_sum = e
            best_min = unconcealed_count
        end
    end

    println(e_sum)
end
