module problem61

sig Number { car: Int, cdr: Int }

fact {
     all n: Number | n.car >= 0 && n.car < 100 &&
         n.cdr >= 0 && n.cdr < 100
}

fun triangle [n: Int] : Int { n fun/mul (n fun/add 1) fun/div 2 }

fun square [n: Int] : Int { n fun/mul n }

fun pentagonal [n: Int] : Int { n fun/mul (3 fun/mul n fun/sub 1) fun/div 2 }

fun hexagonal [n: Int] : Int { n fun/mul (2 fun/mul n fun/sub 1) }

fun heptagonal [n: Int] : Int { n fun/mul (5 fun/mul n fun/sub 3) fun/div 2 }

fun octagonal [n: Int] : Int { n fun/mul (3 fun/mul n fun/sub 2) }

fun to_int [n: Number] : Int { n.car fun/mul 100 fun/add n.cdr }

check { triangle [3] = 6 } for 9 Int

check { some x: to_int [x] = 1015 } for 32 Int
