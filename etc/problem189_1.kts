
// Same as problem189.kts but without constructing the whole list. We
// just count values instead.
//
// It's https://oeis.org/A166736

object Problem189_1 {
  class Graph<T> {
    private val adjacency: MutableMap<T, MutableSet<T>> = mutableMapOf();

    fun hasNode(n: T): Boolean = adjacency.containsKey(n)

    fun hasEdge(from: T, to: T): Boolean = adjacency[from]?.contains(to) ?: false

    fun addNode(n: T) {
      adjacency[n] = mutableSetOf()
    }

    fun addNodes(nodes: Iterable<T>) {
      for (n in nodes) {
        addNode(n)
      }
    }

    fun addEdge(from: T, to: T) {
      if (!hasNode(from) || !hasNode(to)) {
        throw IllegalArgumentException("Nodes $from and $to must already exist in the graph")
      }
      adjacency[from]!!.add(to)
      adjacency[to]!!.add(from)
    }

    fun addEdges(edges: Iterable<Pair<T, T>>) {
      for ((from, to) in edges) {
        addEdge(from, to)
      }
    }

    fun getEdges(from: T): Iterable<T> {
      if (!hasNode(from)) {
        throw IllegalArgumentException("Node $from must already exist in the graph")
      }
      return adjacency[from]!!
    }

    override fun toString(): String =
      "#<Graph: adjacency=$adjacency>"

    val nodes: Iterable<T>
      get() = adjacency.keys

    val edges: Iterable<Pair<T, T>>
      get() = adjacency.flatMap { (u, vs) -> vs.map { v -> u to v } }
  }

  data class Coordinate(val y: Int, val x: Int)

  fun makeGraph(rowCount: Int): Graph<Coordinate> {
    val graph = Graph<Coordinate>()
    for (y in 0 until rowCount) {
      for (x in 0 until 2 * y + 1) {
        graph.addNode(Coordinate(y, x))
        if (x > 0) {
          graph.addEdge(Coordinate(y, x), Coordinate(y, x - 1))
        }
        if (x % 2 == 1) {
          // Link to the node above this one in the graph.
          graph.addEdge(Coordinate(y, x), Coordinate(y - 1, x - 1))
        }
      }
    }
    return graph
  }

  // Colors range from 0 up to (and excluding) `colorCount`.
  fun countValidColorings(graph: Graph<Coordinate>, colorCount: Int): Int {
    // Without loss of generality, assume the top triangle has color 0
    // and the one immediately below it has color 1. Then we'll
    // multiply the solution count by `colorCount * (colorCount - 1)`
    // at the end to compensate. This trick assumes that the graph has
    // at least 2 rows.
    val nodes = graph.nodes.toList().filter { it != Coordinate(0, 0) && it != Coordinate(1, 1) }
    val startingColors = mutableMapOf(Coordinate(0, 0) to 0, Coordinate(1, 1) to 1)
    return countValidColoringsRec(graph, colorCount, startingColors, nodes, 0)
  }

  private fun countValidColoringsRec(graph: Graph<Coordinate>, colorCount: Int, coloring: MutableMap<Coordinate, Int>, nodes: List<Coordinate>, nodesIndex: Int): Int {
    if (nodesIndex >= nodes.size) {
      return 1
    }
    val currentNode = nodes[nodesIndex]
    var count = 0
    for (color in 0 until colorCount) {
      if (graph.getEdges(currentNode).all { coloring[it] != color }) {
        coloring[currentNode] = color
        count += countValidColoringsRec(graph, colorCount, coloring, nodes, nodesIndex + 1)
        coloring.remove(currentNode)
      }
    }
    return count
  }

  fun run() {
    for (rowCount in 2..6) {
      val graph = makeGraph(rowCount)
      val coloringsCount = 6 * countValidColorings(graph, 3)
      println("rowCount=$rowCount coloringsCount=${coloringsCount}")
    }
  }
}

Problem189_1.run()
