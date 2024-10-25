
// Naive brute-force colorings in Kotlin. I'm absolutely certain this
// won't solve the whole problem, but I intend to get some smaller
// sample data and maybe verify that it's not in OEIS.

object Problem189 {
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

  // Gets all colorings, without regard to adjacent nodes. Colors
  // range from 0 up to (and excluding) `colorCount`.
  fun<T> getAllColorings(graph: Graph<T>, colorCount: Int): List<Map<T, Int>> {
    val nodes = graph.nodes.toList()
    val result: ArrayList<Map<T, Int>> = arrayListOf()
    getAllColoringsRec(graph, colorCount, result, mapOf(), nodes, 0)
    return result
  }

  private fun<T> getAllColoringsRec(graph: Graph<T>, colorCount: Int, acc: MutableList<Map<T, Int>>, coloring: Map<T, Int>, nodes: List<T>, nodesIndex: Int) {
    if (nodesIndex >= nodes.size) {
      acc.add(coloring)
      return
    }
    for (color in 0 until colorCount) {
      getAllColoringsRec(graph, colorCount, acc, coloring + (nodes[nodesIndex] to color), nodes, nodesIndex + 1)
    }
  }

  fun<T> isValid(graph: Graph<T>, coloring: Map<T, Int>): Boolean =
    graph.edges.all { (from, to) -> coloring[from] != coloring[to] }

  fun run() {
    for (rowCount in 0..3) {
      val graph = makeGraph(rowCount)
      val allColorings = getAllColorings(graph, 3)
      val validColorings = allColorings.filter { isValid(graph, it) }
      println("rowCount=$rowCount validColorings.size=${validColorings.size}")
    }
  }
}

Problem189.run()
