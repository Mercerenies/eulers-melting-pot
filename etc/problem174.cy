
"Too slow :(" pop

[1000001 { 0, } times ] =ways

3 =n
{ $n 250001 < } {
  $n 2 - =m
  $n print
  { $m 0 > { $n dupe * $m dupe * - dupe =tiles 1000000 <= } { 0 } ? } {
    $ways $tiles ::++
    $m 2 - =m
  } while
  "n" ++
} while

