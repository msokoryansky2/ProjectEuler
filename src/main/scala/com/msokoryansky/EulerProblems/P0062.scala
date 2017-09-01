package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Integer, SearchInput, SearchOutput, StreamSearcher}
import com.msokoryansky.MathUtils.IntegerOps.DigitOps

import scala.collection.immutable.HashSet

/*

The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053).
In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.

 */

class P0062 extends EulerProblem {
  /*
  // Simple brute-force solution that's far too slow:
  def run: String = Integer.ints(1).find(i =>
                      Integer.permutations(i * i * i).filter(_ % 10 != 0).count(ip =>
                        Integer.isPow(ip, 3)) == 3).get.toString
  */

  def run: String = {
    val goal = 5
    def searcher(input: SearchInput[Long, (Long, Map[Long, HashSet[Long]])]):
                          SearchOutput[(Long, Map[Long, HashSet[Long]]), Long] = {
      val state: (Long, Map[Long, HashSet[Long]]) =
        input.state.getOrElse((1, Map[Long, HashSet[Long]]()))
      val newCubes: Map[Long, HashSet[Long]] =
        input.elements.map(_.pow(3)).groupBy(_.sumDigits).map(m => m._1 -> (HashSet() ++ m._2.toSet))
      val oldCubes: Map[Long, HashSet[Long]] =
        state._2
      val allCubes: Map[Long, HashSet[Long]] =
        (newCubes.keys ++ oldCubes.keys).map(hash =>
          hash -> ((if (newCubes.contains(hash)) newCubes(hash) else HashSet[Long]())
            ++ (if (oldCubes.contains(hash)) oldCubes(hash) else HashSet[Long]()))).toMap
      val start: Long =
        state._1.toLong

      // Because permutating digits may create numbers larger than any we've encountered as we keep track of cubes,
      // we only check numbers 3 times less than the max number whose cube we calculated.
      // That way we can be certain that all permutations have already been encountered.
      val firstMatch: Option[Long] = (start until (start + newCubes.size / 3)).toList.find(i => {
          val cube = i * i * i
          val sumDigits = cube.sumDigits
          if (allCubes.contains(sumDigits) &&
              (allCubes(sumDigits).size > goal - 1) &&
              allCubes(sumDigits).count(Integer.isPermutation(_, cube)) == goal) true
          else false
      })

      if (firstMatch.isEmpty) SearchOutput(Some((start + newCubes.size / 3, allCubes)), None)
      else SearchOutput(None, Some(firstMatch.get * firstMatch.get * firstMatch.get))
    }
    StreamSearcher[Long, (Long,Map[Long, HashSet[Long]]), Long](Integer.ints(1), searcher, (_) => false, 21).search.get
      .toString
  }
}

object P0062 extends App {
  (new P0062).printAnswer()
}
