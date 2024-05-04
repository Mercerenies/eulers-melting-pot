
-- Faster solution to Problem 182, that also uses less arrays and
-- fancy shenanigans.
--
-- Claim 1: Given a prime p and a natural number 0 < n < p, the number
-- of solutions to x^n = 1 (mod p) is gcd(n, p - 1).
--
-- Proof 1: https://math.stackexchange.com/a/2552046/84460
--
-- Claim 2: Fix p ≠ q primes and e an integer. The number of
-- unconcealed messages for e (mod p q) is (1 + gcd(e - 1, p - 1)) *
-- (1 + gcd(e - 1, q - 1)).
--
-- Proof 2: Chinese Remainder Theorem tells us that Z / (p q) Z is
-- isomorphic, in a nice way, to (Z / p Z) × (Z / q Z). So look for
-- solutions in Z / p Z and Z / q Z separately, then multiply.
--
-- Now consider Z / p Z. We want to know when m^e = m (mod p).
-- Equivalently, m (m^(e-1) - 1) = 0 (mod p). Z / p Z is a field and
-- thus an integral domain, so we have that either m = 0 or m^(e-1) -
-- 1 = 0 (mod p). The first case gives us one solution, and the second
-- gives us gcd(e - 1, p - 1) (by Claim 1). So a total of 1 + gcd(e -
-- 1, p - 1) solutions.
--
-- Apply the same logic to Z / q Z, so there are 1 + gcd(e - 1, q - 1)
-- solutions in that case. Multiply the two together to get our claim.
--
-- Got it in 2.7 seconds in Lua!
--
-- Further observation: We know gcd(e, phi) = 1 by assumption. And phi
-- is even, so e must be odd for this to hold. Hence, e - 1 is even,
-- go gcd(e - 1, q - 1) and gcd(e - 1, p - 1) are both at least 2. So
-- our total count, for any given (odd) e, of (1 + gcd(e - 1, p - 1))
-- * (1 + gcd(e - 1, q - 1)) is at least 9. And for e = 11, this bound
-- is tight, so we know that the minimum number of unconcealed
-- messages, concretely, is 9. Now we just have to find the e values
-- such that this number is 9. We don't do this in the Lua solution,
-- since Lua is already fast enough.

function gcd(a, b)
  while b ~= 0 do
    a, b = b, a % b
  end
  return a
end

function solutions(p, q, e)
  return (1 + gcd(e - 1, p - 1)) * (1 + gcd(e - 1, q - 1))
end

function findmin(p, q)
  local phi = (p - 1) * (q - 1)
  local bestmin = p * q -- Worst case
  for e = 2, phi - 1 do
    if gcd(e, phi) == 1 then
      bestmin = math.min(bestmin, solutions(p, q, e))
    end
  end
  return bestmin
end

function summatches(p, q)
  local phi = (p - 1) * (q - 1)
  local bestmin = findmin(p, q)
  local sum = 0
  for e = 2, phi - 1 do
    if gcd(e, phi) == 1 and solutions(p, q, e) == bestmin then
      sum = sum + e
    end
  end
  return sum
end

print(summatches(1009, 3643))
