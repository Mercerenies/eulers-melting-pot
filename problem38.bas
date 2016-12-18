
dim values(9)

for i = 1 to 9
    values(i) = 10 - i
next i

while 1
    call cease
    for i = 8 to 1 step -1
        if values(i) > values(i + 1) then
            k = i
            exit for
        end if
    next i
    for i = 9 to k step -1
        if values(k) > values(i) then
            l = i
            exit for
        end if
    next i
    temp = values(k)
    values(k) = values(l)
    values(l) = temp
    call rev k + 1, 9
wend
end

sub rev n, m
    for i = n to m
        j = m + n - i
        if j <= i then
            exit sub
        end if
        temp = values(i)
        values(i) = values(j)
        values(j) = temp
    next i
end sub

sub cease
    val = isCorrect()
    if val then
        print val
        end
    end if
end sub

function isCorrect()
    num = 0
    for i = 1 to 9
        num = num * 10 + values(i)
    next i
    trynum = 0
    for i = 1 to 4
        trynum = trynum * 10 + values(i)
        val$ = ""
        for j = 1 to 9
            val$ = val$ + str$(trynum * j)
            if val(val$) = num then
                isCorrect = num
                exit function
            end if
        next j
    next i
    isCorrect = 0
end function
