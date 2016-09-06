program eulerchallenge
    integer :: i, j, ff, total, cursum, mm, fact
    total = 0
    do i=10,2177280
        cursum = 0
        j = i
        100 if (j /= 0) then
            mm = MOD(j, 10)
            j = j / 10
            fact = 1
            do ff=1,mm
                fact = fact * ff
            enddo
            cursum = cursum + fact
            goto 100
        endif
        if (cursum == i) then
            total = total + i
        endif
    enddo
    print *, total
end program eulerchallenge

