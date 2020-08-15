
# Obviously too slow recursive solution :(

def find_solution_backtracking(answer, best, arr, index)
  return answer, best if index >= arr.length
  loop do
    if arr.sum == arr.reduce(:*)
      if arr.sum < best
        best = arr.sum
        answer = arr.dup
      end
      break
    elsif arr.sum < arr.reduce(:*)
      # No sense in continuing (product > sum)
      break
    elsif arr.sum > best
      # No sense in continuing (sum > best)
      break
    else
      canswer, candidate = find_solution_backtracking(answer, best, arr, index + 1)
      if candidate < best
        answer, best = canswer, candidate
      end
      arr[index] += 1
      (index+1..arr.length-1).each { |i| arr[i] = arr[i - 1] }
    end
  end
  [answer, best]
end

def find_solution(k)
  arr = [1] * k
  arr[k - 2] = 2
  arr[k - 1] = 2
  find_solution_backtracking(nil, Float::INFINITY, arr, 0)
end

def leading_ones(k)
  answer, _ = find_solution k
  answer.count(1)
end

(2..100).each do |i|
  puts "#{i} #{find_solution(i)}"
end
#puts "#{matches.sort.uniq}"
#(2..100).map do |i|
#  i - leading_ones(i)
#end.sort.uniq
