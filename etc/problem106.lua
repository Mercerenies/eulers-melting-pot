
-- A few observations. First, when evaluating Rule 1, we need only
-- consider subset pairs where the two subsets have the same
-- cardinality, for if they have different cardinality then Rule 2
-- already tells us their sums are unequal. Likewise, we needn't
-- consider the case of two singleton sets, since two distinct
-- elements are by definition not equal.
--
-- Once we restrict ourselves to pairs of disjoint subsets of
-- cardinality at least 2, we'll use the following algorithm. Let A be
-- the subset which contains the smallest element of either of the two
-- subsets, and let B be the other subset. Then iterate over all of
-- the elements of the overall set in increasing order. Every time we
-- encounter an element from A, add one. Every time we encounter an
-- element from B, subtract one. If we ever dip below zero, then we
-- have to check this case. If we never dip below zero, then there is
-- an assignment from elements of A to elements of B such that every
-- element of A is strictly less than its counterpart (the algorithm
-- we just used is akin to balancing parentheses and will find such an
-- assignment).

function need_to_check(n, a, b)
  if #a <= 1 or #b <= 1 or #a ~= #b then
    return false -- Sanity checks
  end
  local counter = 0
  local aindex = 1
  local bindex = 1
  for i = 1, n do
    if i == a[aindex] then
      counter = counter + 1
      aindex = aindex + 1
    elseif i == b[bindex] then
      counter = counter - 1
      bindex = bindex + 1
    end
    if counter < 0 then
      return true
    end
  end
  return false
end

function pprint(x)
  io.write("[ ")
  for _, y in ipairs(x) do
    io.write(y .. " ")
  end
  io.write("]")
end

function count_pairs_impl(n, i, a, b)
  if i > n then
    if need_to_check(n, a, b) then
      return 1
    else
      return 0
    end
  end
  local count = 0
  -- Case I: Exclude i from a and b.
  count = count + count_pairs_impl(n, i + 1, a, b)
  -- Case II: Add to a.
  table.insert(a, i)
  count = count + count_pairs_impl(n, i + 1, a, b)
  table.remove(a)
  -- Case III: Add to b (only allowed if a nonempty, to ensure
  -- uniqueness).
  if #a > 0 then
    table.insert(b, i)
    count = count + count_pairs_impl(n, i + 1, a, b)
    table.remove(b)
  end
  return count
end

function count_pairs(n)
  return count_pairs_impl(n, 1, {}, {})
end

print(count_pairs(12))
