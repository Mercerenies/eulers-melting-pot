
// 5 seconds in Swift, not bad.

struct Partition<Element: Hashable> {
    private var subsets: [Element: Subset<Element>]

    init<S>(singletons elements: S) where S: Sequence, S.Element == Element {
        subsets = [:]
        for element in elements {
          subsets[element] = Subset([element])
        }
    }

    func getSubset(_ element: Element) -> [Element] {
      subsets[element]!.elements
    }

    func subsetLength(_ element: Element) -> Int {
      subsets[element]!.elements.count
    }

    mutating func mergeSubsets(_ a: Element, _ b: Element) {
        let aSubset = subsets[a]!
        let bSubset = subsets[b]!
        if aSubset === bSubset {
          return
        }
        if aSubset.elements.count < bSubset.elements.count {
            mergeSubsets(b, a)
            return
        }
        aSubset.elements.append(contentsOf: bSubset.elements)
        for elem in bSubset.elements {
            subsets[elem] = aSubset
        }
    }
}

fileprivate class Subset<Element: Hashable> {
    var elements: [Element]

    init(_ elements: [Element]) {
        self.elements = elements
    }
}

struct LaggedFibonacciState {
    var array: [Int]
    var index: Int

    subscript(_ offset: Int) -> Int {
        get {
            let actualIndex = modulo(index + offset, array.count)
            return array[actualIndex]
        }
        set(newValue) {
            let actualIndex = modulo(index + offset, array.count)
            array[actualIndex] = newValue
        }
    }
}

func modulo(_ a: Int, _ b: Int) -> Int {
  (a % b + b) % b
}

extension Sequence {
    func chunkedPairs() -> some Sequence<(Element, Element)> {
        let iterator = makeIterator()
        return sequence(state: iterator) { iterator in
            guard let a = iterator.next(), let b = iterator.next() else {
                return nil
            }
            return (a, b)
        }
    }
}

func laggedFibonacci() -> some Sequence<Int> {
    let startingArray = (1..<56).map { (k) in (100003 - 200003 * k + 300007 * k * k * k) % 1000000 }
    let state = LaggedFibonacciState(array: startingArray, index: 0)
    return sequence(state: state) { state in
        let currentValue = state[0]
        state[0] = (state[-24] + state[-55]) % 1000000
        state.index += 1
        return currentValue
    }
}

let calls = laggedFibonacci().chunkedPairs().lazy.filter { (a, b) in a != b }
let minister = 524287
var cohorts = Partition(singletons: 0..<1000000)

for (index, data) in calls.enumerated() {
    let (caller, callee) = data
    cohorts.mergeSubsets(caller, callee)
    if cohorts.subsetLength(caller) >= 990000 {
        print(index + 1)
        break
    }
}
