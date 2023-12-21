
# Same as problem172.pl, but translated to Ruby so we can hopefully
# get a bit more speed.

def factorial(n)
  (1..n).inject(1, :*)
end

def multinomial(n, ks)
  remainder = n - ks.sum
  numerator = factorial(n)
  denominator = ks.map { |k| factorial(k) }.inject(:*) * factorial(remainder)
  numerator / denominator
end

def run
  total = 0
  (0..3).each do |ones|
    (0..3).each do |twos|
      (0..3).each do |threes|
        (0..3).each do |fours|
          (0..3).each do |fives|
            next if ones + twos + threes + fours + fives < 3
            (0..3).each do |sixes|
              next if ones + twos + threes + fours + fives + sixes < 6
              (0..3).each do |sevens|
                next if ones + twos + threes + fours + fives + sixes + sevens < 9
                next if ones + twos + threes + fours + fives + sixes + sevens > 18
                (0..3).each do |eights|
                  next if ones + twos + threes + fours + fives + sixes + sevens + eights < 12
                  next if ones + twos + threes + fours + fives + sixes + sevens + eights > 18
                  (0..3).each do |nines|
                    total_nonzero_digits = ones + twos + threes + fours + fives + sixes + sevens + eights + nines
                    next if total_nonzero_digits < 15
                    next if total_nonzero_digits > 18
                    (0..3).each do |zeroes|
                      total_digits = total_nonzero_digits + zeroes
                      next if total_digits != 18
                      digit_counts = [ones, twos, threes, fours, fives, sixes, sevens, eights, nines]
                      total_numbers = multinomial(total_digits, digit_counts)
                      total_with_leading_zeroes = (zeroes > 0) ? multinomial(total_digits - 1, digit_counts) : 0
                      total += (total_numbers - total_with_leading_zeroes)
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end
  total
end

puts run

# 0.8 seconds in Ruby, it's fast. Let's explore.
