package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestStreamSearcher extends FunSuite {
  test("search finds desired result in a stream or gives up as instructed. " +
    "Search stream of integers in chunks of 7 for a pair of integers each divisible by 5 that add up to 40.") {
    def searcher(input: SearchInput[Long, List[Long]]): SearchOutput[List[Long], (Long, Long)] = {
      val newDiv5 = input.elements.filter(_ % 5 == 0)
      val allDiv5 = if (input.state.isEmpty) newDiv5 else newDiv5 ++ input.state.get
      val match1 = newDiv5.find(n => allDiv5.exists(a => a < n && a + n == 40))
      if (match1.isEmpty) SearchOutput(Some(allDiv5), None)
      else SearchOutput(None, Some((match1.get, allDiv5.find(a => a < match1.get && a + match1.get == 40).head)))
    }

    def stopper(n: Long): Boolean = n > 28
    assert(StreamSearcher[Long, List[Long], (Long, Long)](Integer.ints(1), searcher, stopper, 7).search.get === (25, 15))
    assert(StreamSearcher[Long, List[Long], (Long, Long)](Integer.ints(1), searcher, stopper, 1).search.get === (25, 15))
    assert(StreamSearcher[Long, List[Long], (Long, Long)](Integer.ints(1), searcher, stopper, 8).search.get === (25, 15))
    assert(StreamSearcher[Long, List[Long], (Long, Long)](Integer.ints(1), searcher, stopper, 100).search.get === (25, 15))

    def stopper2(n: Long): Boolean = n > 24
    assert(StreamSearcher[Long, List[Long], (Long, Long)](Integer.ints(1), searcher, stopper2, 7).search.isEmpty)
  }
}
