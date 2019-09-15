dnl
dnl count(str, ch)
define(`count',`dnl
len(patsubst($1, [^$2], `'))dnl
')dnl
dnl
dnl _sqrt(n, curr)
define(`_sqrt', `ifelse(eval(($2 ** 2) > $1), 1, eval($2 - 1), `_sqrt($1, eval($2 + 1))')')dnl
dnl
dnl sqrt(n)
define(`sqrt', `_sqrt($1, 0)')dnl
dnl
dnl period_recurse(s)
define(`period_recurse', `dnl
define(`idx', index(triplets, [m d a]))dnl
ifelse(idx, -1, `dnl
define(`triplets', triplets [m d a])dnl
define(`m', eval(d * a - m))dnl
define(`d', eval(($1 - m ** 2) / d))dnl
define(`a', eval((a0 + m) / d))dnl
period_recurse($1)dnl
', `dnl
eval(count(triplets, [) - count(substr(triplets, 0, idx), [))dnl
')dnl
')dnl
dnl
dnl frac_period(s)
define(`frac_period', `dnl
define(`triplets')dnl
define(`a0', sqrt($1))dnl
ifelse(eval(a0 ** 2), $1, 0, `dnl
define(`m', 0)dnl
define(`d', 1)dnl
define(`a', a0)dnl
period_recurse($1)dnl
')dnl
')dnl
dnl
dnl loop(n)
define(`loop', `dnl
ifelse(eval(frac_period($1) % 2), 1, `define(`total', incr(total))')dnl
ifelse(eval($1 < 10000), 1, `loop(eval($1 + 1))')dnl
')dnl
dnl
define(`total', 0)dnl
loop(1)dnl
total`'
