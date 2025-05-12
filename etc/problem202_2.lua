
-- Dumbed-down variant of problem202_1.rs, in preparation for translation.

local y = 2
local count = 0
while 2 * y < 6008819575 do
  if ((y - 2) / 3) % 10000000 == 0 then
    print(y)
  end
  if y % 5 == 0 or y % 11 == 0 or y % 17 == 0 or y % 23 == 0 or y % 29 == 0 or y % 41 == 0 or y % 47 == 0 then
    -- Skip
  else
    count = count + 1
  end
  y = y + 3
end
print(2 * count)
