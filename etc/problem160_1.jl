
# Runs in 3 seconds. Keep an array of all of the factors (mod 10^5) we
# need. Count them in a clever way, keeping out any multiples of five
# systematically. Then we go back and remove a factor of 2 for every
# factor of 5. In this way, we systematically remove all 10's (since
# 10 = 2*5), and we have all of the factors of our factorial, modulo
# 10^5, excluding the tens (hence, not ending in a zero). Multiply
# them all out, using powermod for efficient exponentiation in our
# modular arithmetic scope, and you have the answer.
#
# It's an easy proof to show that there will always be more powers of
# 2 than powers of 5, provided our upper limit is at least, you know,
# 2.

LIMIT = 1_000_000_000_000  # Inclusive limit

DIGIT_COUNT = 5
MODULO = 10 ^ DIGIT_COUNT

NON_FIVES = filter((x) -> x % 5 != 0, 1:(MODULO-1))

function update_multiples(multiples::Array, power_of_5::Integer)
    for i in NON_FIVES
        last_main_answer = floor(LIMIT ÷ power_of_5, digits=-DIGIT_COUNT)
        multiples[i] += LIMIT ÷ (power_of_5 * MODULO)
        if last_main_answer + i <= LIMIT ÷ power_of_5
            multiples[i] += 1
        end
    end
end

function count_all_fives()
    total = 0
    power_of_5 = 5
    while power_of_5 < LIMIT
        total += LIMIT ÷ power_of_5
        power_of_5 *= 5
    end
    return total
end

function remove_fives(value::Integer)
    while value % 5 == 0
        value ÷= 5
    end
    return value
end

let
    multiples = zeros(Int, MODULO)

    # First, count every factor (modulo 10^5) that goes into
    # 1,000,000,000!. Explicitly do NOT count the powers of 5.
    # Basically, count every factor with its fives divided out (modulo
    # 10^5).
    power_of_5 = 1
    while power_of_5 <= LIMIT
        update_multiples(multiples, power_of_5)
        power_of_5 *= 5
    end

    # Now divide out twos from the first several multiples of 2. Let N
    # = count_all_fives(). We want to remove a single power of 2 from
    # each of [2, 4, 6, 8, ... 2N-2, 2N]. But remember that these
    # numbers have already had their fives removed, so we have to take
    # that into consideration when removing values.
    values_to_remove = count_all_fives()
    i = 1
    while values_to_remove > 0
        current_value = NON_FIVES[i]
        old_value = (current_value * 2) % MODULO
        new_value = current_value % MODULO
        multiples[old_value] -= (LIMIT ÷ (2 * MODULO))
        multiples[new_value] += (LIMIT ÷ (2 * MODULO))
        values_to_remove -= LIMIT ÷ (2 * MODULO)
        i += 1
    end

    # If we removed too many values in our wholesale removal, then add
    # some back by hand.
    i = 1
    while values_to_remove < 0
        current_value = NON_FIVES[i]
        old_value = current_value % MODULO
        new_value = (current_value * 2) % MODULO
        multiples[old_value] -= 1
        multiples[new_value] += 1
        values_to_remove += 1
        i += 1
    end

    # Now we have all the multiples. Multiply it out.
    product = 1
    for (factor, count) in enumerate(multiples)
        product = (product * powermod(factor, count, MODULO)) % MODULO
    end
    println(product)
end
