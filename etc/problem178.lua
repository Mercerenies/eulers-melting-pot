
-- Straightforward recursion with caching.
--
-- Count the total number of step numbers of length <= 40. Then
-- subtract those that omit 0 and (separately) subtract those that
-- omit 9. Add back in those that omit 0 and 9 (which were
-- double-counted, per inclusion-exclusion principle). There's your
-- answer. With a cache table, this runs in < 100ms in Lua.
--
-- Also, the number of step numbers is https://oeis.org/A090994, but
-- it's stated without proof.

cache = {}

function count_step(len, lower, upper)
  if len < 1 then
    return 0
  end
  local total = 0
  for i=math.max(lower, 1),upper do
    total = total + count_step_rec(len - 1, i, lower, upper)
  end
  return total
end

function count_step_rec(len, last_digit, lower, upper)
  local key = len .. "," .. last_digit .. "," .. lower .. "," .. upper
  if cache[key] then
    return cache[key]
  end
  if len == 0 then
    return 1
  else
    local incremented = (last_digit == upper and 0 or count_step_rec(len - 1, last_digit + 1, lower, upper))
    local decremented = (last_digit == lower and 0 or count_step_rec(len - 1, last_digit - 1, lower, upper))
    cache[key] = incremented + decremented
    return cache[key]
  end
end

count = 0
for i=10,40 do
  count = count + count_step(i, 0, 9) - count_step(i, 1, 9) - count_step(i, 0, 8) + count_step(i, 1, 8)
end
print(count)

