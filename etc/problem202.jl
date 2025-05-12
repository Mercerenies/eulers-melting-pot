
# The equilateral triangle from the problem description.
#
# A-----B
#  \   /
#   \ /
#    C
#
# Artificially define the point C to be (0, 0) in our coordinate grid.
# Further define the distance (in y-coordinates) from C to the AB line
# to be 1. This means the side length of our equilateral triangle is 2
# sqrt(3) / 3, but we don't ever need that quantity.
#
# A few notes:
#
# 1. Excluding the case where we fire a laser straight up at the very
# beginning, the laser will never be directly vertical. Proof: Just
# draw out the diagram and see where the alleged line came from. The
# alleged line (which is now perpendicular to AB) must have, one
# bounce ago, been perpendicular to either AC or BC. Hence, it didn't
# come from C.
#
# 2. The laser's path will never be directly horizontal. Proof: The
# only way we can get a horizontal line is by having a laser that's
# infinitely looping in an equilateral triangle, and therefore it
# never came from any of the vertex points.
#
# 3. All lasers that enter *and* exit at C must hit the point (0, -1).
# Proof: Any laser which enters and exits at C must follow a
# horizontally-symmetrical path. Consider the halfway point of the
# laser's life. The halfway point must clearly be at X = 0. If Y is
# not -1, then we're not currently bouncing off of any walls, so the
# only way to preserve symmetry is if the laser currently has slope 0.
# But that's a horizontal line, which cannot occur, per proposition
# (2).

import Base: +, -, *

struct Point
    x::Float64
    y::Float64
end

x(p::Point) = p.x
y(p::Point) = p.y

*(p::Point, r::Number) = Point(p.x * r, p.y * r)
*(r::Number, p::Point) = Point(p.x * r, p.y * r)
+(p1::Point, p2::Point) = Point(p1.x + p2.x, p1.y + p2.y)
-(p1::Point, p2::Point) = Point(p1.x - p2.x, p1.y - p2.y)

struct Line
    slope::Float64
    y_intercept::Float64
end

Line(origin::Point, dx, dy) = Line(dy / dx, origin.y - origin.x * dy / dx)
Line(p1::Point, p2::Point) = Line(p1, p2.x - p1.x, p2.y - p1.y)

slope(line::Line) = line.slope
y_intercept(line::Line) = line.y_intercept
origin(line::Line) = Point(0, y_intercept(line))
dx(line::Line) = 1
dy(line::Line) = slope(line)

apply(line::Line, x) = y_intercept(line) + slope(line) * x

function intersection(s1::Line, s2::Line)
    x = (y_intercept(s2) - y_intercept(s1)) / (slope(s1) - slope(s2))
    y = apply(s1, x)
    Point(x, y)
end

function reflect(path::Line; off::Line)
    # https://stackoverflow.com/a/78762794/2288659
    a = slope(path)
    b = y_intercept(path)
    c = slope(off)
    d = y_intercept(off)
    m = (2 * c + a * (c * c - 1)) / (2 * a * c + 1 - c * c)
    y = (2 * d * (a * c + 1) - b * (c * c + 1)) / (2 * a * c + 1 - c * c)
    Line(m, y)
end

struct Reflection
    line::Line
    next_hit::String  # "AB", "AC", "BC"
end

ab = Line(Point(0, -1), 1, 0)
ac = Line(Point(0, 0), - sqrt(3) / 3, 1)
bc = Line(Point(0, 0), sqrt(3) / 3, 1)

triangle_lines = Dict("AB" => ab, "AC" => ac, "BC" => bc)
vertices = Dict(
    "AB" => Point(0, 0),
    "AC" => Point(sqrt(3) / 3, 1),
    "BC" => Point(-sqrt(3) / 3, 1),
)

function iterate(refl::Reflection)
    line_to_intersect = triangle_lines[refl.next_hit]
    new_line = reflect(refl.line, off = line_to_intersect)
    # /////
end

let
    println(ac)
    s1 = Line(Point(-3, 0), Point(-2, 1))
    s2 = Line(Point(3, 0), Point(2, 1))
    println(intersection(s1, s2))
    println(s1)
    println(reflect(s1, off = s2))
    println(reflect(s1, off = Line(0, 3)))
    println(s2)
    println(reflect(s1, off = Line(0.1, 3)))
end
