
"Too slow :(" pop

{ &< { pop } { swap pop } ? } =min
{ &> { pop } { swap pop } ? } =max

{ dupe dupe * + swap dupe dupe * + * 4 / :i } =count_aa_rectangles

{
  2 * =width 2 * =height 0 =total
  0 =x
  {
    0 =y
    {
      $x $y + 2 % 0 >< {
        $y $width $x - min =upper
        $width $y + $x - $height - 1 + 1 max $upper 1 + min =mid_bound
        .total $height $y - $mid_bound 1 + * +=
        .total $width $x - $upper $mid_bound - 1 + * $upper $upper 1 + * $mid_bound $mid_bound 1 - * - 2 / :i - +=
      } if
      .y ++
    } $height times
    .x ++
  } $width times
  $total
} =count_dia_rectangles

0
1 =i
{
  $i print
  1 =j
  {
    $i $j count_aa_rectangles +
    $i $j count_dia_rectangles +
    .j ++
  } 43 times
  .i ++
} 47 times
print
