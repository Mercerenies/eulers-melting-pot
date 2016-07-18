package main

import (
    "fmt"
)

var memo map[int]bool

func isAbundant(val int) bool {
    if _, ok := memo[val]; !ok {
        sum := 0
        for x := 1; x < val; x++ {
            if val % x == 0 {
                sum += x
            }
        }
        memo[val] = (sum > val)
    }
    return memo[val]
}

func main() {
    memo = map[int]bool{}
    total := 0
    for attempt := 1; attempt <= 28123; attempt++ {
        canBeSummed := false
        for addend := 1; addend < attempt; addend++ {
            if isAbundant(addend) && isAbundant(attempt - addend) {
                canBeSummed = true
                break
            }
        }
        if !canBeSummed {
            total += attempt
        }
    }
    fmt.Println(total)
}
