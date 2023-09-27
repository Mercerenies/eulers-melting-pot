
"Partial solution: Already too slow"

"Define a rational number as a list [numerator, denominator]"
"Define a point as a list [a, b] of rational numbers"
"Define a line segment as a list [a, b] of points"

function! Gcd(x, y)
    let l:x = a:x
    let l:y = a:y
    while l:y != 0
        let l:tmp = l:x
        let l:x = l:y
        let l:y = l:tmp % l:x
    endwhile
    return l:x
endfunction

function! Fraction(numer, denom)
    let l:numer = a:numer
    let l:denom = a:denom
    if l:denom < 0
        let l:denom = l:denom * -1
        let l:numer = l:numer * -1
    endif
    let l:d = Gcd(abs(l:numer), abs(l:denom))
    return [l:numer / d, l:denom / d]
endfunction

function! Add(p, q)
    return Fraction(a:p[0] * a:q[1] + a:p[1] * a:q[0], a:p[1] * a:q[1])
endfunction

function! Cmp(p, q)
    return a:p[0] * a:q[1] - a:q[0] * a:p[1]
endfunction

function! Sub(p, q)
    return Fraction(a:p[0] * a:q[1] - a:q[0] * a:p[1], a:p[1] * a:q[1])
endfunction

function! Mul(p, q)
    return Fraction(a:p[0] * a:q[0], a:p[1] * a:q[1])
endfunction

function! Div(p, q)
    return Fraction(a:p[0] * a:q[1], a:p[1] * a:q[0])
endfunction

function! Rsgn(p)
    if a:p.numerator < 0
        return -1
    elseif a:p.numerator > 0
        return 1
    else
        return 0
    endif
endfunction

function! Point(first, second)
    return [a:first, a:second]
endfunction

function! CmpPoint(a, b)
    if Sub(a:a[0], a:b[0]) != [0, 1]
        return Rsgn(Sub(a:a[0], a:b[0]))
    else
        return Rsgn(Sub(a:a[1], a:b[1]))
    endif
endfunction

function! LineSegment(first, second)
    if CmpPoint(a:first, a:second) > 0
        return [a:second, a:first]
    else
        return [a:first, a:second]
    endif
endfunction

function! IsVertical(segment)
    return Cmp(a:segment[0][0], a:segment[1][0]) == 0
endfunction

function! Slope(segment)
    let l:x1 = segment[0][0]
    let l:y1 = segment[0][1]
    let l:x2 = segment[1][0]
    let l:y2 = segment[1][1]
    return Div(Sub(l:y2, l:y1), Sub(l:x2, l:x1))
endfunction

echo CmpPoint([[0, 1], [0, 1]], [[0, 1], [0, 1]])
