package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Integer, SearchInput, SearchOutput, StreamSearcher}

/*

The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053).
In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.

 */

class P0062 extends EulerProblem {
  def run: String = {
    def searcher(input: SearchInput[Long, (Long, Map[Long, Long])]): SearchOutput[(Long, Map[Long, Long]), Long] = {
      val state: (Long, Map[Long, Long]) = input.state.getOrElse((0, Map[Long, Long]()))
      val newCubes: Map[Long, Long] = input.elements.map(i => (i, i * i * i)).toMap
      val oldCubes: Map[Long, Long] = state._2
      val allCubes: Map[Long, Long] = newCubes ++ oldCubes
      val allCubeValues = allCubes.values.toList
      val start: Long = state._1.toLong

      // Because permutating digits may create numbers larger than any we've encountered as we keep track of cubes,
      // we only check numbers 3 times less than the max number whose cube we calculated.
      // That way we can be super-sure that all permutations have already been encountered.
      val firstMatch: Option[Long] = (start until (start + newCubes.size / 3)).toList.find(i => {
          println(i)
          Integer.permutations(i * i * i).filterNot(_ % 10 == 0).count(ip => allCubeValues.contains(ip)) == 3})

      if (firstMatch.isEmpty) SearchOutput(Some((start + newCubes.size / 3, allCubes)), None)
      else SearchOutput(None, Some(firstMatch.get))
    }
    StreamSearcher[Long, (Long, Map[Long, Long]), Long](Integer.ints(1), searcher, (_) => false, 300).search.get.toString
  }
}

object P0062 extends App {
  (new P0062).printAnswer()
}
