
-- No bignum support :(

local function gcd(a, b)
  while b ~= 0 do
    a, b = b, a % b
  end
  return a
end

local upperlimit = 1000001

local primes = {}
for n=1,upperlimit do
  primes[n] = true
end
primes[1] = false

for i=2,upperlimit do
  if primes[i] then
    local n = i + i
    while n < upperlimit do
      primes[n] = false
      n = n + i
    end
  end
end

local function phi(n)
  if primes[n] then
    return n - 1
  end
  local prod = n
  for p=2,math.sqrt(n) do
    if primes[p] then
      if n % p == 0 then
        prod = prod * (1 - 1 / p)
        if p * p ~= n and primes[n / p] then
          prod = prod * (1 - p / n)
        end
      end
    end
  end
  return prod
end

local sum = 0
for i=2,1000000 do
  sum = sum + phi(i)
end
print(sum)
