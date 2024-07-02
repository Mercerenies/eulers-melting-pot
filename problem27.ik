
pdict = dict()

isPrime = method(p,
  if(pdict key?(p),
    pdict[p],
    pdict[p] = case(p,
      or(0, 1), false,
      2, true,
      else, (2..(p-1)) takeWhile(val, val * val <= p) all?(val, p % val != 0),
    )
  )
)

maxa = 0
maxb = 0
streak = 0
(-999..999) each(a,
  (-1000..1000) each(b,
    curr = 0
    while(isPrime((curr * curr + a * curr + b) abs),
      curr++)
    if(curr > streak,
      maxa = a
      maxb = b
      streak = curr
    )
  )
)
(maxa * maxb) println
