
pdict = dict()

Number isPrime = method(
  if(pdict key?(self),
    pdict[self],
    pdict[self] = case(self,
      fn(x, x < 2), false,
      2, true,
      else, (2..(self-1)) takeWhile(val, val * val <= self) all?(val, self % val != 0),
    )
  )
)

maxa = 0
maxb = 0
streak = 0
(2..1000) filter(isPrime) each(b,
  b println
  (-999..999) each(a,
    curr = 0
    while((curr * curr + a * curr + b) isPrime,
      curr++)
    if(curr > streak,
      maxa = a
      maxb = b
      streak = curr
    )
  )
)
(maxa * maxb) println
