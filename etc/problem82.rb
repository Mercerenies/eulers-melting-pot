
Width, Height = 80, 80

arr = []
File.open("./files/p082_matrix.txt") do |f|
  f.each_line do |line|
    arr << line.split(',').map(&:to_i)
  end
end

acc = arr.map(&:dup)

# Nothing to be done for the first column; the starting values are already correct.
(1..Width-1).each do |x|
  (0..Height-1).each do |y|
    mincost = Float::INFINITY
    (0..Height-1).each do |y0|
      # Calculate the cost of starting at (x, y0), going right once,
      # and then going up/down as necessary to get to y.
      cost = acc[y0][x-1]
      a, b = [y0, y].minmax
      (a..b).each { |curr| cost += arr[curr][x] }
      mincost = [mincost, cost].min
    end
    acc[y][x] = mincost
  end
end

puts acc.map { |row| row[-1] }.min
