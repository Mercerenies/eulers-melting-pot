
pdict = dict()

isPrime = method(x0,
                 if(pdict key?(x0),
                    pdict[x0],
                    if(x0 < 1,
                       false,
                       isP = true
                       (2..(x0-1)) select(val,
                                          if(x0 % val == 0,
                                             isP = false,
                                             nil))
                       pdict[x0] = isP)))

maxa = 0
maxb = 0
streak = 0
(-999..999) select(a,
                   (-999..999) select(b,
                                      curr = 0
                                      while(isPrime((curr * curr + a * curr + b) abs),
                                            curr++)
                                      if(curr > streak,
                                         maxa = a
                                         maxb = b
                                         streak = curr,
                                         nil)))
(maxa * maxb) println
