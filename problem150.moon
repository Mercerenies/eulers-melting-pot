
class Triangle

  new: (@height) =>
    @data = {}
    for i = 1, @height
      @data[i] = {}
      for j = 1, i
        @data[i][j] = 0

  get: (row, column) =>
    @data[row][column]

  put: (row, column, value) =>
    @data[row][column] = value

  generate: ->
    triangle = Triangle(1000)
    row = 1
    column = 1
    t = 0
    for _ = 1, 500500
      t = (615949 * t + 797807) % (1 << 20)
      triangle.data[row][column] = t - (1 << 19)
      column += 1
      if column > row
        row += 1
        column = 1
    triangle

  partial_sums: =>
    sum_triangle = Triangle(@height)
    for y = 1, @height
      sum = 0
      for x = 1, y
        sum += @get(y, x)
        sum_triangle\put(y, x, sum)
    sum_triangle

triangle = Triangle.generate()
sum_triangle = triangle\partial_sums!
best_sum = 500500 * 2 ^ 19 -- Largest possible sum, since each cell is bounded above by 2^19.

for y = 1, 1000
  for x = 1, y
    partial_sum = 0
    for h = 0, 1000 - y - 1
      partial_sum += sum_triangle\get(y + h, x + h)
      if x > 1
        partial_sum -= sum_triangle\get(y + h, x - 1)
      best_sum = math.min(best_sum, partial_sum)
print(best_sum)
