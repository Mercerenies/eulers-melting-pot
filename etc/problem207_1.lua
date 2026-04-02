
-- Per the observations in problem207.hs, we want to enumerate w and
-- calculate `k = w (w - 1)`.
--
-- P(m) is defined as the proportion of partitions which are perfect
-- with k <= m. As m increases, the proportion only adds partitions
-- (both perfect and imperfect), never subtracts. That is, P(m + n) is
-- P(m) with some values added to the numerator and denominator. We
-- want the smallest m such that P(m) < 1/12345. P(m) is usually
-- decreasing and only increases as a perfect partition. So step one
-- would be finding the pair of perfect partitions between which the
-- solution lies.
--
-- Let w > 1 be a positive integer. Then let x be the number of powers
-- of two less than w. Then P(w (w - 1)) = x / (w - 1) [Note: 'minus
-- one' comes from the fact that w = 1 is not considered a valid
-- solution since k = w(w - 1) would be zero]. These are also the only
-- values for which P changes (it is piecewise constant on all other
-- values).
--
-- Direct calculation:
--
--      w | P(w (w - 1))
-- -------+-------------
--      2 |  1 / 1
--      4 |  2 / 3
--      8 |  3 / 7
--     16 |  4 / 15
--     32 |  5 / 31
--     64 |  6 / 63
--    128 |  7 / 127
--    256 |  8 / 255
--    512 |  9 / 511
--   1024 | 10 / 1023
--   2048 | 11 / 2047
--   4096 | 12 / 4095
--   8192 | 13 / 8191
--  16384 | 14 / 16383
--  32768 | 15 / 32767
--  65536 | 16 / 65535
-- 131072 | 17 / 131071
-- 262144 | 18 / 262143  <- Less than 1/12345
--
-- By direct paper calculation, the solution m is between 4^17 - 2^17
-- and 4^18 - 2^18.
--
-- So we want the smallest w with m = w (w - 1) such that P(m) = 17 /
-- (w - 1) < 1/12345. So w > 12345*17 + 1. Since w is an integer, this
-- gives w = 12345*17 + 2. Then our solution m = w (w - 1).

local n = 1
local twon = 2

while n / (twon - 1) > 1 / 12345 do
  n = n + 1
  twon = twon * 2
end


local w = 12345 * (n - 1) + 2
local m = w * (w - 1)
print(m)
