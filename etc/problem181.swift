
// countAll runs in 21 seconds and is correct. :)
//
// countBlack was just a prototype and is not used in the final solution.

// Ignoring the "white objects" part and just counting black objects, to get a feel for the problem.
func countBlack(of remaining: Int) -> Int { // A000041 (Partition Numbers)
    countBlack(of: remaining, last: remaining)
}

func countBlack(of remaining: Int, last lastGroup: Int) -> Int {
    if remaining <= 0 {
        return 1
    } else {
        var total = 0
        for i in stride(from: min(lastGroup, remaining), through: 1, by: -1) {
            total += countBlack(of: remaining - i, last: i)
        }
        return total
    }

}

for i in 1...15 {
    print("i = \(i), there are \(countBlack(of: i, last: i)) ways to sum")
}

// Now count all objects. To make sure order doesn't matter, we
// require that the lists be monotonically-decreasing, using the
// natural ordering on (white, black) as the total ordering.
struct CacheEntry: Hashable {
    let white: Int
    let black: Int
    let lastWhite: Int
    let lastBlack: Int
}
var countCache = [CacheEntry: Int]()

func countAll(white: Int, black: Int) -> Int {
    countAll(white: white, black: black, lastWhite: white, lastBlack: black)
}

func countAll(white: Int, black: Int, lastWhite: Int, lastBlack: Int) -> Int {
    let cacheEntry = CacheEntry(white: white, black: black, lastWhite: lastWhite, lastBlack: lastBlack)
    if white == 0 && black == 0 {
        return 1
    } else if let cacheHit = countCache[cacheEntry] {
        return cacheHit
    } else {
        var total = 0
        for whiteSpent in stride(from: min(white, lastWhite), through: 0, by: -1) {
            let blackLimit = whiteSpent == lastWhite ? min(black, lastBlack) : black
            for blackSpent in stride(from: blackLimit, through: 0, by: -1) {
                if whiteSpent == 0 && blackSpent == 0 {
                    continue
                }
                total += countAll(
                  white: white - whiteSpent,
                  black: black - blackSpent,
                  lastWhite: whiteSpent,
                  lastBlack: blackSpent
                )
            }
        }
        countCache[cacheEntry] = total
        return total
    }
}

print(countAll(white: 40, black: 60))
