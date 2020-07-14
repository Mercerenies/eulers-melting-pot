# coding: utf-8

=begin
  00 - GO   01 - A1   02 - CC1  03 - A2   04 - T1
  05 - R1   06 - B1   07 - CH1  08 - B2   09 - B3
  10 - JAIL 11 - C1   12 - U1   13 - C2   14 - C3
  15 - R2   16 - D1   17 - CC2  18 - D2   19 - D3
  20 - FP   21 - E1   22 - CH2  23 - E2   24 - E3
  25 - R3   26 - F1   27 - F2   28 - U2   29 - F3
  30 - G2J  31 - G1   32 - G2   33 - CC3  34 - G3
  35 - R4   36 - CH3  37 - H1   38 - T2   39 - H2
=end

require 'matrix'

Epsilon = 0.000001

class AssertionError < RuntimeError
end

def assert
  raise AssertionError unless yield
end

def type_of_space(n)
  case n
  when 30 then :gotojail
  when 2, 17, 33 then :chest
  when 7, 22, 36 then :chance
  else :trivial
  end
end

def gauss(a)
  #https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode
  h, k = 0, 0
  m, n = a.row_count, a.column_count
  while h < m and k < n
    i_max = (h..m-1).max_by { |i| a[i, k].abs }
    if a[i_max, k] == 0
      k = k + 1
    else
      # Swap rows
      (0..n-1).each { |j| a[h, j], a[i_max, j] = a[i_max, j], a[h, j] }
      (h+1..m-1).each do |i|
        f = a[i, k] / a[h, k]
        a[i, k] = 0
        (k+1..n-1).each do |j|
          a[i, j] = a[i, j] - a[h, j] * f
        end
      end
      h += 1
      k += 1
    end
  end
end

def find_pivots(a)
  pivots = []
  (0...a.row_count).each do |i|
    pivotcol = nil
    (0...a.column_count).each do |j|
      if a[i, j].abs > Epsilon
        pivotcol = j
        break
      end
    end
    pivots << pivotcol
  end
  pivots
end

def solve_eigenvector(a, lambda)
  # We want to solve (A-λI)x=0, so subtract λ from the diagonals.
  a = a.dup
  assert { a.row_count == a.column_count }
  (0...a.row_count).each { |i| a[i, i] -= lambda }
  gauss a
  pivots = find_pivots(a)
  result = [1] * a.column_count
  (0...a.row_count).reverse_each do |i|
    pivot = pivots[i]
    if pivot
      value = 0
      (pivot+1...a.column_count).each do |j|
        value -= a[i, j] * result[j]
      end
      value /= a[i, pivot]
      result[pivot] = value
    end
  end
  result
end

def normalize_vector(v)
  magnitude = v.sum
  v.map { |x| x / magnitude }
end

# Roll the dice
standard_movement = Matrix.build(120, 120) do |r, c|
  # Gaussian distribution (two 6-sided dice)
  if r < 40
    v = 0
    # All non-doubles route here
    v += case (r - c) % 40
         when 3, 7 then 2.0 / 16
         when 4, 6 then 2.0 / 16
         when 5    then 4.0 / 16
         else 0.0
         end
    # Also, going to jail ends you up here on doubles
    if c >= 80 and r == 10
      v += 4.0 / 16
    end
    v
  else
    v = 0
    # You only get here if you rolled doubles from the below state
    if (r / 40).to_i == (c / 40).to_i + 1
      case (r - c) % 40
      when 2, 4, 6, 8 then v += 1.0 / 16
      end
    end
    v
  end
end

space_resolution = Matrix.build(120, 120) do |r, c|
  if (r / 40).to_i != (c / 40).to_i
    # Space resolution never changes the doubles counter
    0.0
  else
    r = r % 40
    c = c % 40
    case type_of_space c
    when :gotojail
      # Always go to jail :)
      if r == 10 then 1.0 else 0.0 end
    when :chest
      case r
      when 0 then 1.0 / 16.0 # Go
      when 10 then 1.0 / 16.0 # Jail
      when c then 14.0 / 16.0
      else 0.0
      end
    when :chance
      v = 0
      # Standard movements
      v += case r
           when 0 then 1.0 / 16.0 # Go
           when 10 then 1.0 / 16.0 # Jail
           when 11 then 1.0 / 16.0 # C1
           when 24 then 1.0 / 16.0 # E3
           when 39 then 1.0 / 16.0 # H2
           when 5 then 1.0 / 16.0 # R1
           else 0.0
           end
      # "Next railway"
      if r % 10 == 5 and (r - c) % 40 < 10
        v += 2.0 / 16.0
      end
      # "Next utility" (Yes, I'm brute forcing these calculation)
      case c
      when 7, 36
        v += 1.0 / 16.0 if r == 12 # U1
      when 22
        v += 1.0 / 16.0 if r == 28 # U2
      else raise RuntimeError.new "Missed a chance tile!"
      end
      # "Go back three squares"
      v += 1.0 / 16.0 if r - c == -3
      # The Chance tiles that don't move you
      v += 6.0 / 16.0 if r == c
      v
    when :trivial
      if r == c then 1.0 else 0.0 end
    end
  end
end

(0...120).each do |j|
  n = (0...120).map { |i| space_resolution[i, j] }.sum
  raise RuntimeError.new "Column #{j} sums to #{n}" unless n == 1
end

transition_matrix = space_resolution * standard_movement

# We want the eigenvector corresponding to λ = 1. That is, we want a
# nontrivial solution x to (A-I)x=0. So subtract one from the
# diagonals.
vector = solve_eigenvector transition_matrix, 1
vector = normalize_vector vector

collated = (0...40).map { |n| vector[n] + vector[n + 40] + vector[n + 80] }

(0...40).each do |n|
  case type_of_space n
  when :chest    then print "CC   "
  when :chance   then print "CH   "
  when :gotojail then print "G2J  "
  else
    case n
    when 10 then print "JAIL "
    when 0  then print "GO   "
    else print "     "
    end
  end
  printf "% .3f", collated[n]
  puts
end

sorted = (0...40).to_a.sort_by { |n| - collated[n] }
printf "%02d%02d%02d\n", sorted[0], sorted[1], sorted[2]
