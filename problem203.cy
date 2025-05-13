
1 1 >< =true
1 1 <> =false

"( ? ? -- ? )" pop
{ { pop $true } { } ? } =or

"( ? ? -- ? )" pop
{ { } { pop $false } ? } =and

"( x lst -- ? )" pop
{ swap =x $false swap { $x >< or } each } =contains

"( n -- ? )" pop
"Note: Optimization in the while loop: We divide by i whenever it divides the number once; this makes the squarefree function WAAAAY faster" pop
{ =n 2 =i { $i $i * $n <= $n $i $i * % 0 <> and } { $n $i % 0 >< { $n $i / =n } { } ? $i 1 + =i } while $n $i $i * % 0 <> } =squarefree

"( a b -- a b a )" pop
{ =b dupe $b swap } =over

"Bury top of stack on bottom of stack" pop
{ =buryval rev $buryval rev } =bury

"( x -- x ) Evaluates and appends to visited if needed" pop
{ dupe squarefree { dupe $visited contains { } { dupe $visited swap <~ } ? } { } ? } =evaluate

"Assumes it has the whole stack and does shenanigans; variable $c shall be the number of Pascal elements currently on the stack" pop
{ 1 bury { evaluate over + bury } $c 1 - times pop 1 bury $c 1 + =c } =iterate

[ ] =visited

1 =c

1 { iterate } 51 times

$visited {+} fold print
