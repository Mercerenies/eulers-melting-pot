
limit: 100000

allsums: limit#0

(limit-2) {
  i: x
  d: $[9!i;9;9!i]
  allsums[i-2]: allsums[i] | d
  1+i
}/ 2

allsums[2]