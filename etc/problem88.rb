
# Obviously too slow recursive solution :(

def find_solution_backtracking(best, arr, index)
  return best if index >= arr.length
  loop do
    if arr.sum == arr.reduce(:*)
      best = [best, arr.sum].min
      break
    elsif arr.sum < arr.reduce(:*)
      # No sense in continuing (product > sum)
      break
    elsif arr.sum > best
      # No sense in continuing (sum > best)
      break
    else
      candidate = find_solution_backtracking(best, arr, index + 1)
      best = [best, candidate].min
      arr[index] += 1
      (index+1..arr.length-1).each { |i| arr[i] = arr[i - 1] }
    end
  end
  best
end

def find_solution(k)
  arr = [1] * k
  arr[k - 2] = 2
  arr[k - 1] = 2
  find_solution_backtracking(Float::INFINITY, arr, 0)
end

matches = []
(2..100).each do |i|
  matches << find_solution(i)
end
puts "#{matches.sort.uniq}"
(2..100).each do |i|
  puts "#{i} #{find_solution i}"
end
