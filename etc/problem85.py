
# Every rectangle can be uniquely decomposed into a vertical part and
# a horizontal part. The base cases where w = 1 or h = 1 are just the
# triangular numbers, so we decompose each rectangle into its
# 1-dimensional components, count the number of those, and multiply to
# get the result.

def rectangles(w, h):
    return (w * (w + 1) // 2) * (h * (h + 1) // 2)

# Our (very poor) initial guess is a 1x1 rectangle, which is 1,999,999
# off.
bestw, besth = 1, 1
bestvalue = 1999999

w, h = 1, 1
while True:
    curr = rectangles(w, h)
    if abs(curr - 2000000) < bestvalue:
        bestw, besth = w, h
        bestvalue = abs(curr - 2000000)
    if curr > 2000000:
        if w == 1:
            # We're too big in both directions; abort
            break
        else:
            w = 1
            h += 1
    else:
        w += 1

print(bestw * besth)

"""
We're doing this stack based...

TOP
1
h+1
bestarea
bestvalue
2000000
BOTTOM

AT TOP (4 t 4 t) QUESTION MARK

TOP
(curr-2000000)
2000000
curr
h
w
bestvalue
bestarea
BOTTOM

START OF LOOP

TOP
w
h
bestarea
bestvalue
2000000
BOTTOM

"""
