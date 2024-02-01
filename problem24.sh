#!/bin/tcsh

set remaining=1000000
set nums=0123456789
set result=

set factorials='1 2 6 24 120 720 5040 40320 362880'

foreach dotimes (1 2 3 4 5 6 7 8 9)
    set loopvar=0
    set length=`printf $nums | wc -c`
    @ length=$length - 1
    foreach x (`echo $nums | grep -o .`)
        @ loopvar=$loopvar + 1
        set poss=`echo $factorials | tr " " "\n" | head -n $length | tail -n 1`
        @ newresult=$remaining - $poss
        if ($newresult > 0) then
            set remaining=$newresult
        else
            set result=$result$x
            @ loopmin=$loopvar - 1
            @ loopmax=$length - $loopvar + 2
            set nums=`echo $nums | head -c $loopmin``echo $nums | tail -c $loopmax`
            break
        endif
    end
end
set result=$result$nums
echo $result
