
dnl count(str, ch)
define(`count', `
  len(patsubst($1, [^$2], `'))
')

dnl _sqrt(n, curr)
define(`_sqrt', `ifelse(eval(($2 ** 2) > $1), 1, eval($2 - 1), `_sqrt($1, eval($2 + 1))')')

dnl sqrt(n)
define(`sqrt', `_sqrt($1, 0)')

dnl period_recurse(s)
define(`period_recurse', `
  define(`idx', index(triplets, [m d a]))
  ifelse(idx, -1, `
    define(`triplets', triplets [m d a])
    define(`m', eval(d * a - m))
    define(`d', eval(($1 - m ** 2) / d))
    define(`a', eval((a0 + m) / d))
    period_recurse($1)
  ', `
    eval(count(triplets, [) - count(substr(triplets, 0, idx), [))
  ')
')

dnl frac_period(s)
define(`frac_period', `
  define(`triplets')
  define(`a0', sqrt($1))
  ifelse(eval(a0 ** 2), $1, 0, `
    define(`m', 0)
    define(`d', 1)
    define(`a', a0)
    period_recurse($1)
  ')
')

dnl loop(n)
define(`loop', `
  ifelse(eval(frac_period($1) % 2), 1, `define(`total', incr(total))')
  ifelse(eval($1 < 10000), 1, `loop(eval($1 + 1))')
')

define(`total', 0)
loop(1)
total
