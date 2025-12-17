
# Dumbing down problem205.rs in preparation for Inform 7. Our arrays
# are only allowed to add and remove, not to insert.
#
# Dumbing down problem205_1.rb even more to try to get floating point
# numbers to agree with us.
#
# Scratch that, we're not using floats. Where we're going, floats
# can't save us.

TOTAL = [1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] # = 12,230,590,464

def integer_to_bin(x, length)
  res = []
  length.times do
    res << (x % 2)
    x = x / 2
  end
  res.reverse
end

# Note: Assumes both numbers have the same length (as arrays). Output
# will have that length. Any extra bits are truncated.
def add_integers(a, b)
  result = []
  carry = 0
  for i in 0...a.length
    a_digit = a[- i - 1] || 0
    b_digit = b[- i - 1] || 0
    result << ((a_digit + b_digit + carry) % 2)
    carry = (a_digit + b_digit + carry) / 2
  end
  result.reverse
end

def twos_complement(a)
  a_flipped = a.map { |x| 1 - x }
  add_integers(a_flipped, [1])
end

def subtract_integers(a, b)
  add_integers(a, twos_complement(b))
end

def greater_than_or_equal(a, b)
  diff = subtract_integers(a, b)
  diff[0] == 0
end

# Assumes a and b are integers and a << b. Outputs *only* bits after
# the decimal point. Clobbers a.
def division(a, b, output_prec)
  digits = []
  output_prec.times do
    a.shift  # Keep the numbers the same length
    a << 0
    if greater_than_or_equal(a, b)
      digits << 1
      a = subtract_integers(a, b)
    else
      digits << 0
    end
  end
  digits
end

# Assumption: We have at least 29 binary digits.
def bin_to_decimal(frac_digits)
  (
    frac_digits[ 0] * 500_000_000 +
    frac_digits[ 1] * 250_000_000 +
    frac_digits[ 2] * 125_000_000 +
    frac_digits[ 3] *  62_500_000 +
    frac_digits[ 4] *  31_250_000 +
    frac_digits[ 5] *  15_625_000 +
    frac_digits[ 6] *   7_812_500 +
    frac_digits[ 7] *   3_906_250 +
    frac_digits[ 8] *   1_953_125 +
    frac_digits[ 9] *     976_562 +
    frac_digits[10] *     488_281 +
    frac_digits[11] *     244_140 +
    frac_digits[12] *     122_070 +
    frac_digits[13] *      61_035 +
    frac_digits[14] *      30_517 +
    frac_digits[15] *      15_258 +
    frac_digits[16] *       7_629 +
    frac_digits[17] *       3_814 +
    frac_digits[18] *       1_907 +
    frac_digits[19] *         953 +
    frac_digits[20] *         476 +
    frac_digits[21] *         238 +
    frac_digits[22] *         119 +
    frac_digits[23] *          59 +
    frac_digits[24] *          29 +
    frac_digits[25] *          14 +
    frac_digits[26] *           7 +
    frac_digits[27] *           3 +
    frac_digits[28] *           1
  )
end

def print_dec(dec_digits)
  p "0.#{dec_digits}"
end

thirty_four = integer_to_bin(34, 8)
ten = integer_to_bin(10, 8)
p division(ten, thirty_four, 10)
p print_dec(bin_to_decimal([1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]))
exit

possible_peter_wins = Array.new(37, 0)
for peter_a in 1..4
  for peter_b in 1..4
    for peter_c in 1..4
      for peter_d in 1..4
        for peter_e in 1..4
          for peter_f in 1..4
            for peter_g in 1..4
              for peter_h in 1..4
                for peter_i in 1..4
                  peter = peter_a + peter_b + peter_c + peter_d + peter_e + peter_f + peter_g + peter_h + peter_i
                  for i in 0..(peter - 1)
                    n = possible_peter_wins[i]
                    possible_peter_wins.delete_at(i)
                    possible_peter_wins.insert(i, n + 1)
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

p possible_peter_wins

peter_wins = 0.0
for colin_a in 1..6
  for colin_b in 1..6
    for colin_c in 1..6
      for colin_d in 1..6
        for colin_e in 1..6
          for colin_f in 1..6
            colin = colin_a + colin_b + colin_c + colin_d + colin_e + colin_f
            peter_wins += possible_peter_wins[colin] / TOTAL
          end
        end
      end
    end
  end
end

p peter_wins

# Inform 7 notes: e is not a valid var name (b/c it's a math constant), so we use ee.
