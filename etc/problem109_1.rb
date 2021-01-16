
# The other one works. I'm trying to distill it down now into its
# parts, to get rid of all of the higher-level language constructs.

def score_is_double(idx)
  (idx >= 21 and idx <= 40) or idx == 62
end

def score_from_index(idx)
  case idx
  when 61 then 25
  when 62 then 50
  when 0 then 0
  else
    ((idx - 1) % 20 + 1) * ((idx - 1) / 20 + 1)
  end
end

# We're going to use dynamic programming on a massive table. The
# "state" is going to be (1) the number of points remaining, (2) the
# max number of darts left to be thrown, and (3) the position in the
# $scores array of the last element.
$cache = {}

def count(points, darts, score_index)
  state = 63 * (4 * points + darts) + score_index
  # Check obvious conditions first
  return 0 if points < 0
  # Check cache
  if not $cache.include?(state)
    result = nil
    if darts == 0
      # If no darts left, then we're done
      if points == 0 and score_is_double(score_index)
        result = 1
      else
        result = 0
      end
    else
      result = 0
      # We could hit basically any dart on the board
      score_index = 0 if darts == 1 # Last dart can be anything (no ambiguity)
      (score_index..62).each do |idx|
        result += count(points - score_from_index(idx), darts - 1, idx)
      end
    end
    $cache[state] = result
  end
  $cache[state]
end

$total = 0
(1..99).each do |s|
  $total += count(s, 3, 0)
end
puts $total
