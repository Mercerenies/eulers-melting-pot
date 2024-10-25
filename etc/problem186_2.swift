
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

func nextElem(_ state: inout LaggedFibonacciState) -> Int {
    let currentValue = state[0]
    state[0] = (state[-24] + state[-55]) % 1000000
    state.index += 1
    return currentValue
}

var stateArray = Array(repeating: 0, count: 55)
for i in 1..<56 {
  stateArray[i - 1] = (100003 - 200003 * i + 300007 * i * i * i) % 1000000
}
var state = LaggedFibonacciState(array: stateArray, index: 0)

let minister = 524287
var cohorts = Partition(singletons: 0..<1000000)
var index = 0

while true {
    let caller = nextElem(&state)
    let callee = nextElem(&state)
    if caller == callee {
        continue
    }
    cohorts.mergeSubsets(caller, callee)
    if cohorts.subsetLength(minister) >= 990000 {
        print(index + 1)
        break
    }
    index += 1
}
