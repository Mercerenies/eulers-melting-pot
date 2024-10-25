
// Runs in 3 minutes. I think I can do better. Stay tuned :)

import Foundation

struct Graph<Node: Hashable> {
    var edges: [Node: [Node]]

    init<S: Sequence>(_ vertices: S) where S.Element == Node {
        edges = [:]
        for v in vertices {
            edges[v] = []
        }
    }

    mutating func addEdge(_ from: Node, _ to: Node) {
        if !(edges[from]!.contains(to)) {
            edges[from]!.append(to)
            edges[to]!.append(from)
        }
    }

    func outEdges(_ from: Node) -> [Node] {
        edges[from]!
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

func visit<T>(graph: Graph<T>, from node: T, _ visited: inout Set<T>) {
    if visited.contains(node) {
        return
    }
    var frontier: Set = [node]
    while !frontier.isEmpty {
        let currentNode = frontier.removeFirst()
        visited.insert(currentNode)
        frontier.formUnion(graph.outEdges(currentNode).filter { !visited.contains($0) })
    }
}

let calls = laggedFibonacci().chunkedPairs().lazy.filter { (a, b) in a != b }
var ministersFriends: Set = [524287]
var friendGraph = Graph(0..<1000000)

for (index, data) in calls.enumerated() {
    let (caller, callee) = data
    friendGraph.addEdge(caller, callee)
    if ministersFriends.contains(caller) && !ministersFriends.contains(callee) {
      visit(graph: friendGraph, from: callee, &ministersFriends)
    } else if ministersFriends.contains(callee) && !ministersFriends.contains(caller) {
      visit(graph: friendGraph, from: caller, &ministersFriends)
    }

    if ministersFriends.count >= 990000 {
        print(index + 1)
        break
    }
}
