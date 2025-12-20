
# Dumbing down problem205.rs in preparation for Inform 7. Our arrays
# are only allowed to add and remove, not to insert.

TOTAL = 12230590464

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

peter_wins = 0
for colin_a in 1..6
  for colin_b in 1..6
    for colin_c in 1..6
      for colin_d in 1..6
        for colin_e in 1..6
          for colin_f in 1..6
            colin = colin_a + colin_b + colin_c + colin_d + colin_e + colin_f
            peter_wins += possible_peter_wins[colin]
          end
        end
      end
    end
  end
end

p peter_wins.to_f / TOTAL

# Inform 7 notes: e is not a valid var name (b/c it's a math constant), so we use ee.
#
# Other Inform 7 notes: division is right associative. yes, you read that right.
