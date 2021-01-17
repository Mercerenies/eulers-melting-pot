
# Now it's time to try something even dumber.

$results = []

(0..25451).each do |state|
  score_index = state % 63
  darts = (state / 63) % 4
  points = state / 252

  if darts == 0
    if points == 0 and ((score_index >= 21 and score_index <= 40) or score_index == 62)
      $results.push(1)
    else
      $results.push(0)
    end
  else
    result = 0
    score_index = 0 if darts == 1
    (score_index..62).each do |idx|
      this_dart_score = case idx
                        when 61 then 25
                        when 62 then 50
                        when 0 then 0
                        else ((idx - 1) % 20 + 1) * ((idx - 1) / 20 + 1)
                        end
      points1 = points - this_dart_score
      if points1 >= 0
        result += $results[63 * (4 * points1 + (darts - 1)) + idx]
      end
    end
    $results.push(result)
  end

end

$total = 0
(1..99).each do |s|
  $total += $results[63 * (4 * s + 3)]
end
puts $total

# Befunge stuff (y, x)
# First line is the global array
# Second / third lines have local variables:
#  (1, 0) - 25451 (constant)
#  (1, 1) - state
#  (1, 2) - score_index
#  (1, 3) - darts
#  (1, 4) - points
#  (1, 5) - total
#  (1, 6) - s
#  (1, 7) - idx
#  (1, 8) - (swap space 1)
#  (1, 9) - (swap space 2)
#
# Stack is empty at global loop start
#
# STACK TOP
#
# result
#
# STACK BOTTOM
#
# Swap: 81p91p81g91g (The regular swap instruction is broken in the interpreter)
