
var
  phi: array[1..1000000, float]
  n: int
  sum: int

for i in 1..1000000:
  phi[i] = float(i)

for i in 2..1000000:
  if phi[i] == float(i):
    n = i
    while n <= 1000000:
      phi[n] = phi[n] * (1 - 1 / float(i))
      n += i

sum = 0
for i in 2..1000000:
  sum += int(phi[i])

echo sum