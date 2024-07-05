
// Same as problem186_1.swift but simplified down to NOT use OOP class
// shenanigans for identity.

struct Partition<Element: Hashable> {
    private var subsetIndices: [Element: Int]
    private var subsets: [[Element]]

    init<S>(singletons elements: S) where S: Sequence, S.Element == Element {
        subsetIndices = [:]
        subsets = Array(repeating: [], count: 1000000)
        for (index, element) in elements.enumerated() {
          subsetIndices[element] = index
          subsets[index].append(element)
        }
    }

    func subsetLength(_ element: Element) -> Int {
      let index = subsetIndices[element]!
      return subsets[index].count
    }

    mutating func mergeSubsets(_ a: Element, _ b: Element) {
        let aSubsetIndex = subsetIndices[a]!
        let bSubsetIndex = subsetIndices[b]!
        if aSubsetIndex == bSubsetIndex {
          return
        }
        if subsets[aSubsetIndex].count < subsets[bSubsetIndex].count {
            mergeSubsets(b, a)
            return
        }
        subsets[aSubsetIndex].append(contentsOf: subsets[bSubsetIndex])
        for elem in subsets[bSubsetIndex] {
            subsetIndices[elem] = aSubsetIndex
        }
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
