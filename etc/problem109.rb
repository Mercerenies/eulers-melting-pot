
class Score
  include Comparable

  attr_reader :base_score, :multiplier

  def initialize(base_score, multiplier)
    @base_score = base_score
    @multiplier = multiplier
  end

  def self.[](base_score, multiplier)
    Score.new(base_score, multiplier)
  end

  def score
    base_score * multiplier
  end

  def to_s
    mult = case multiplier
           when 1 then 'S'
           when 2 then 'D'
           when 3 then 'T'
           else ''
           end
    "#{mult}#{base_score}"
  end

  def double?
    multiplier == 2
  end

  def <=>(other)
    m = self.multiplier <=> other.multiplier
    if m != 0
      m
    else
      self.base_score <=> other.base_score
    end
  end

end

$scores = []
# Miss
$scores += [Score[0, 0]]
# Outer ring
(1..20).each do |base|
  $scores += (1..3).map { |mult| Score[base, mult] }
end
# Bullseye
$scores += [Score[25, 1], Score[25, 2]]
# Sort to avoid ambiguity
$scores.sort!

# We're going to use dynamic programming on a massive table. The
# "state" is going to be (1) the number of points remaining, (2) the
# max number of darts left to be thrown, and (3) the position in the
# $scores array of the last element.
$cache = {}

def count(acc, points, darts, score_index)
  state = [points, darts, score_index]
  # Check cache
  if not $cache.include?(state)
    result = nil
    if points < 0
      # Failure :(
      result = 0
    elsif darts == 0
      # If no darts left, then we're done
      if points == 0 and $scores[score_index].double?
        #puts "#{acc.map(&:to_s)}"
        result = 1
      else
        result = 0
      end
    else
      result = 0
      # We could hit basically any dart on the board
      score_index = 0 if darts == 1 # Last dart can be anything (no ambiguity)
      (score_index..$scores.length-1).each do |idx|
        score = $scores[idx]
        acc.push(score)
        result += count(acc, points - score.score, darts - 1, idx)
        acc.pop()
      end
    end
    $cache[state] = result
  end
  $cache[state]
end

$total = 0
(1..99).each do |s|
  $total += count([], s, 3, 0)
end
puts $total
