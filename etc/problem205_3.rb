
# Dumbing down problem205.rs in preparation for Inform 7. Our arrays
# are only allowed to add and remove, not to insert.
#
# Dumbing down problem205_1.rb even more to try to get floating point
# numbers to agree with us.
#
# Scratch that, we're not using floats. Where we're going, floats
# can't save us.
#
# INLINE EVERYTHING! ALL MUST BE ASSIMILATED

TOTAL = [0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] # = 12,230,590,464
TOTAL_DIGITS = 35

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

DIGITS = 40
peter_wins = [0] * DIGITS
for colin_a in 1..6
  for colin_b in 1..6
    for colin_c in 1..6
      for colin_d in 1..6
        for colin_e in 1..6
          for colin_f in 1..6
            colin = colin_a + colin_b + colin_c + colin_d + colin_e + colin_f
            n = possible_peter_wins[colin]
            numerator = []
            TOTAL_DIGITS.times do
              numerator << (n % 2)
              n = n / 2
            end
            numerator.reverse!
            quotient = []
            DIGITS.times do
              numerator.shift
              numerator << 0
              b_flipped = TOTAL.map { |x| 1 - x }
              diff = []
              carry = 1
              for i in 0...TOTAL_DIGITS
                a_digit = numerator[- i - 1]
                b_f_digit = b_flipped[- i - 1]
                diff << ((a_digit + b_f_digit + carry) % 2)
                carry = (a_digit + b_f_digit + carry) / 2
              end
              diff.reverse!
              if diff[0] == 0
                quotient << 1
                numerator = diff
              else
                quotient << 0
              end
            end
            new_peter_wins = []
            carry = 0
            for i in 0...DIGITS
              a_digit = peter_wins[- i - 1]
              b_digit = quotient[- i - 1]
              new_peter_wins << ((a_digit + b_digit + carry) % 2)
              carry = (a_digit + b_digit + carry) / 2
            end
            new_peter_wins.reverse!
            peter_wins = new_peter_wins
          end
        end
      end
    end
  end
end

peter_wins_dec = 0
peter_wins_dec += peter_wins[ 0] * 500_000_000
peter_wins_dec += peter_wins[ 1] * 250_000_000
peter_wins_dec += peter_wins[ 2] * 125_000_000
peter_wins_dec += peter_wins[ 3] *  62_500_000
peter_wins_dec += peter_wins[ 4] *  31_250_000
peter_wins_dec += peter_wins[ 5] *  15_625_000
peter_wins_dec += peter_wins[ 6] *   7_812_500
peter_wins_dec += peter_wins[ 7] *   3_906_250
peter_wins_dec += peter_wins[ 8] *   1_953_125
peter_wins_dec += peter_wins[ 9] *     976_562
peter_wins_dec += peter_wins[10] *     488_281
peter_wins_dec += peter_wins[11] *     244_140
peter_wins_dec += peter_wins[12] *     122_070
peter_wins_dec += peter_wins[13] *      61_035
peter_wins_dec += peter_wins[14] *      30_517
peter_wins_dec += peter_wins[15] *      15_258
peter_wins_dec += peter_wins[16] *       7_629
peter_wins_dec += peter_wins[17] *       3_814
peter_wins_dec += peter_wins[18] *       1_907
peter_wins_dec += peter_wins[19] *         953
peter_wins_dec += peter_wins[20] *         476
peter_wins_dec += peter_wins[21] *         238
peter_wins_dec += peter_wins[22] *         119
peter_wins_dec += peter_wins[23] *          59
peter_wins_dec += peter_wins[24] *          29
peter_wins_dec += peter_wins[25] *          14
peter_wins_dec += peter_wins[26] *           7
peter_wins_dec += peter_wins[27] *           3
peter_wins_dec += peter_wins[28] *           1
# Round off to 7 places
peter_wins_dec += 50
peter_wins_dec = peter_wins_dec / 100

# Assumes the first digit is nonzero, which we all know it is.
print "0.#{peter_wins_dec}"

# Inform 7 notes: e is not a valid var name (b/c it's a math constant), so we use ee.
