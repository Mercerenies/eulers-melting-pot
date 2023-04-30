
data class Paper(
  val size: Int,
) {

  fun subdivide(): List<Paper> =
    listOf(Paper(size + 1), Paper(size + 1))

  // Cut the paper up as much as needed to get a paper of size N, then
  // remove one of the size N ones.
  //
  // Precondition: size <= n
  fun removeSizeN(n: Int): List<Paper> =
    (size+1..n).map { Paper(it) }

}

fun<T> List<T>.removed(index: Int): List<T> {
    return this.filterIndexed { i, _ -> i != index }
}

val TARGET_SIZE: Int = 5

data class Envelope(
  val papers: List<Paper>,
) {

  companion object {

    val INITIAL: Envelope = Envelope(listOf(Paper(1)))

    private val cache: HashMap<Envelope, Double> = HashMap()

  }

  // Remove the Nth element from the papers list, cut it to get to the
  // target size, put any unused papers back in, and return the new list.
  fun removeAndCutFromList(index: Int, targetSize: Int): Envelope {
    val chosenPaper = papers[index]
    val remainingPapers = papers.removed(index) + chosenPaper.removeSizeN(targetSize)
    return Envelope(remainingPapers)
  }

  // Calculate all ways to consume all papers, keeping track of the
  // expected value of the number of times we find that there's only
  // one paper in the envelope. This is a recursive computation.
  fun consumeAll(): Double {
    if (papers.isEmpty()) {
      // Base case: The day is done
      return 0.0;
    } else if (cache.containsKey(this)) {
      return cache[this]!!
    } else {
      var expectedValue = 0.0;
      if (papers.size == 1) {
        // There's one paper left right now, so add one to the
        // expected value (we're guaranteed to be in a situation where
        // we do this at least once)
        expectedValue += 1
      }
      // Now remove each index in turn.
      val length = papers.size
      for (i in papers.indices) {
        val newEnvelope = removeAndCutFromList(i, TARGET_SIZE)
        val innerExpectedValue = newEnvelope.consumeAll()
        expectedValue += innerExpectedValue / length
      }
      cache[this] = expectedValue
      return expectedValue
    }
  }

}

fun main() {
  // Subtract 2, since we're guaranteed to get a single sheet the
  // first and last time, and the problem description says not to
  // include those cases.
  println("%.6f".format(Envelope.INITIAL.consumeAll() - 2))
}
