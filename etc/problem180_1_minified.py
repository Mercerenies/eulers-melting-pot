from fractions import Fraction as F
s=set()
q={x*x:x for x in range(36)}
def Q(r):
 n=q.get(r.numerator);d=q.get(r.denominator)
 if None not in(n,d):return F(n,d)
B=lambda z:z and 0<z.numerator<z.denominator<36
def f(x,y):
 z=x+y
 if B(z):s.add(x+y+z)
 z=Q(x*x+y*y)
 if B(z):s.add(x+y+z)
 z=x*y/(x+y)
 if B(z):s.add(x+y+z)
 z=Q(x*x*y*y/(x*x+y*y))
 if B(z):s.add(x+y+z)
def e():
 for a in range(1,36):
  for b in range(a+1,36):yield F(a,b)
for x in e():
 for y in e():f(x, y)
t=sum(s);print(t.numerator+t.denominator)
