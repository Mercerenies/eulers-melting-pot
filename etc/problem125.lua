
--function is_palindrome(n)
--  local s = tostring(n)
--  return s == string.reverse(s)
--end

-- More complicated is_palindrome but doesn't use strings, so it may
-- be more applicable in some languages.
function is_palindrome(n)
  if n % 10 == 0 then
    return false
  end
  local m = 0
  while n ~= 0 do
    if n == m then
      return true
    end
    if n // 10 == m then
      return true
    end
    m = 10 * m + n % 10
    n = n // 10
  end
  return false
end

limit = 99999999

counted = {}
total = 0
for i = 1,limit do
  sum = i * i
  for j = i+1,limit do
    sum = sum + j * j
    if sum > limit then
      break
    end
    if is_palindrome(sum) and not counted[sum] then
      counted[sum] = 1
      total = total + sum
    end
  end
end
print(total)

--[[
TAPE
-2 m (tmp)
-1 n (tmp)
0 limit
1 total
2 i
3 j
4 sum
5 k

GRID
Row -1 is counted array; -1 is unvisited, 0 is visited

TAPE POSITION
0
(all loops set tape position to zero before loop start)

STACK (TOP)

OUTER LOOP STACK
(empty)

INNER LOOP STACK
(empty)

INNER INNER LOOP STACK
sum

--]]

-- ///// Clever stuff with cumulative sums?
