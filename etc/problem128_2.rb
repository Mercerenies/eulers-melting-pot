
# Same algorithm at problem128_1.scala but written without any
# higher-level stuff. Just pure imperative code.

def is_prime(i)
  # return false if i < 2 # (Unnecessary the way we're calling this)
  (2..Math.sqrt(i)).each do |j|
    return false if i % j == 0
  end
  true
end

remaining = 1998
i = 2
result = nil
loop do
  if i % 5 != 0

    # Starting value
    starting = true
    starting = false unless is_prime(6 * i + 1)
    starting = false unless is_prime(6 * i - 1)
    starting = false unless is_prime(12 * i + 5)
    if starting
      remaining -= 1
      if remaining == 0
        result = 3 * i * i - 3 * i + 2
        break
      end
    end

    # Ending value
    ending = true
    ending = false unless is_prime(6 * i - 1)
    ending = false unless is_prime(6 * i + 5)
    ending = false unless is_prime(12 * i - 7)
    if ending
      remaining -= 1
      if remaining == 0
        result = 3 * i * i + 3 * i + 1
        break
      end
    end

  end
  i += 1
end
puts result

# LOCALS:
# 0 - i
# 1 - remaining
#
#
# Working prime number checker
#
# e~c;e-$v   PRIME
#       >v
#        e
#        -
#        $
#       ^<
#
#        N
#        O
#        T
#        P
#        R
#        I
#        M
#        E

