
ln = 0
ld = 1

for d in range(1, 1000001):
    if (3 * d) % 7 == 0:
        continue
    n = (3 * d) // 7
    if ln * d < n * ld:
        ln = n
        ld = d

print(ln) # Can assume ln / ld not reducible because if it were, we'd
          # have already found the "better" solution first (since we
          # start with smaller denominator. Hence, gcd(ln, ld) = 1
          # already.

# Chef not viable due to lack of comparisons. Taxi?

# FRONT ln, ld BACK at Joyless Park
# d is stored at Cyclone

# At [main loop], we're at Cyclone and there should be two d's waiting
# there.
