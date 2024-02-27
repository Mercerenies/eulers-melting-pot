(F:=__import__('fractions').Fraction)
(s:=set())
(q:={x*x:x for x in range(36)})
(Q:=lambda r:[n:=q.get(r.numerator),d:=q.get(r.denominator),None not in(n,d)and F(n,d)][-1])
(B:=lambda z:z and 0<z.numerator<z.denominator<36)
(f:=lambda x,y:[z:=x+y,B(z)and s.add(x+y+z),z:=Q(x*x+y*y),B(z)and s.add(x+y+z),z:=x*y/(x+y),B(z)and s.add(x+y+z),z:=Q(x*x*y*y/(x*x+y*y)),B(z)and s.add(x+y+z)])
(e:=[F(a,b)for a in range(1,36)for b in range(a+1,36)])
[f(x,y)for x in e for y in e]
[t:=sum(s),print(t.numerator+t.denominator)]
