
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
