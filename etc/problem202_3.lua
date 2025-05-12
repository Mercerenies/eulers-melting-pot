
-- Inclusion-exclusion on prime factors

-- Limit is inclusive; assumes k != 0 (mod 3) and only returns
-- multiples of k which are = k (mod 3)
function multiples_up_to(k, limit)
  local total_multiples = math.floor(limit / k)
  return math.floor((total_multiples + 2) / 3)
end

-- Limit is inclusive; assumes k != 0 (mod 3) and only returns
-- multiples of k which are != k (mod 3)
function multiples_up_to_opp(k, limit)
  local total_multiples = math.floor(limit / k)
  return math.floor((total_multiples + 1) / 3)
end

local factors = {5, 11, 17, 23, 29, 41, 47}

local upper = 6008819575
local total = 0

-- Add all candidates
total = total + multiples_up_to_opp(1, upper)

-- Subtract singles
for a = 1, 7 do
  total = total - multiples_up_to(factors[a], upper)
end

-- Add doubles
for a = 1, 7 do
  for b = a + 1, 7 do
    total = total + multiples_up_to_opp(factors[a] * factors[b], upper)
  end
end

-- Subtract triples
for a = 1, 7 do
  for b = a + 1, 7 do
    for c = b + 1, 7 do
      total = total - multiples_up_to(factors[a] * factors[b] * factors[c], upper)
    end
  end
end

-- Add quads
for a = 1, 7 do
  for b = a + 1, 7 do
    for c = b + 1, 7 do
      for d = c + 1, 7 do
        total = total + multiples_up_to_opp(factors[a] * factors[b] * factors[c] * factors[d], upper)
      end
    end
  end
end

-- Subtract 5-tuples
for a = 1, 7 do
  for b = a + 1, 7 do
    for c = b + 1, 7 do
      for d = c + 1, 7 do
        for e = d + 1, 7 do
          total = total - multiples_up_to(factors[a] * factors[b] * factors[c] * factors[d] * factors[e], upper)
        end
      end
    end
  end
end

-- Add 6-tuples
for a = 1, 7 do
  for b = a + 1, 7 do
    for c = b + 1, 7 do
      for d = c + 1, 7 do
        for e = d + 1, 7 do
          for f = e + 1, 7 do
            total = total + multiples_up_to_opp(factors[a] * factors[b] * factors[c] * factors[d] * factors[e] * factors[f], upper)
          end
        end
      end
    end
  end
end

-- Subtract 7-tuple
total = total - multiples_up_to(1201763915, upper)

print(total)
