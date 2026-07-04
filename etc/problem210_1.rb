
# I made a mess of the geometry. See 210_diagram.png for details, but
# I think Region (*) is wrong (the others are probably fine).

def triangular(k)
  k * (k + 1) / 2
end

def points_interior_to_rect(a, b)
  # Figuring this out is just a bit of a tedious evens-and-odds
  # problem. The recurrence is:
  #
  # f(a, 1) = a
  # f(1, b) = b
  # f(a, b) = (2b - 1) + f(a - 1, b)
  #         = (2a - 1) + f(a, b - 1)
  2 * a * b - a - b + 1
end

SQRT2 = Math.sqrt(2)

class ObtuseTriangleCounter
  attr_reader :r

  def initialize(r)
    @r = r
  end

  # Region (*)
  def right_triangle_points
    # We only count interior points, so it's offset a little bit, but
    # it *is* the triangular numbers.
    hypotenuse = r.ceildiv(4)
    s = if hypotenuse <= 2
          0
        else
          triangular(hypotenuse - 2)
        end
    # Also count the points *on* the right triangle, but not the
    # corner itself.
    if hypotenuse > 1
      s += 2 * (hypotenuse - 1)
    end
    s
  end

  # Region (**)
  def positive_rect_points
    # Region (**) is a rectangle of height r/4 and width r/2. We also
    # include two of the sides of this rectangle (the boundary
    # conditions of the problem)
    a = r.ceildiv(4)
    b = r.ceildiv(2)
    s = points_interior_to_rect(a, b) + (a + b)
    if a > 0 and b > 0  # We double-counted the corner
      s -= 1
    end
    s
  end

  # Region (***)
  def negative_rect_points
    # Region (***) is a square of side length r/2. We also include two
    # of the sides of this rectangle (the boundary conditions of the
    # problem)
    a = r.ceildiv(2)
    s = points_interior_to_rect(a, a) + 2 * a
    if a > 0
      s -= 1
    end
    s
  end

  def all_points
    p [right_triangle_points, positive_rect_points, negative_rect_points]
    (right_triangle_points + positive_rect_points + negative_rect_points) * 2 # + (r - 12)
  end
end

def n(i)
  ObtuseTriangleCounter.new(i).all_points
end

#puts "n(24) = #{n(24)}"
#puts "n(28) = #{n(28)}"
#puts "n(32) = #{n(32)}"
(1..6).each do |i|
  puts "n(#{i*4}) = #{n(i*4)}"
end

puts n(1_000_000_000)
