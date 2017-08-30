package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Integer, SearchInput, SearchOutput, StreamSearcher}

import scala.collection.immutable.HashSet

/*

The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053).
In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.

 */

class P0062 extends EulerProblem {
  /*
  def run: String = Integer.ints(1).find(i => {
                      println(i)
                      val cube = i * i * i
                      Integer.permutations(cube).filter(_ % 10 != 0).count(ip =>
                        Integer.isPow(ip, 3)) == 3}).get.toString
  */

  def run: String = {
    def searcher(input: SearchInput[Long, (Long, HashSet[Long])]): SearchOutput[(Long, HashSet[Long]), Long] = {
      val state: (Long, HashSet[Long]) = input.state.getOrElse((0, HashSet[Long]()))
      val newCubes: HashSet[Long] = HashSet[Long]() ++ input.elements.map(i => i * i * i).toSet
      val oldCubes: HashSet[Long] = state._2
      val allCubes: HashSet[Long] = newCubes ++ oldCubes
      val start: Long = state._1.toLong

      // Because permutating digits may create numbers larger than any we've encountered as we keep track of cubes,
      // we only check numbers 3 times less than the max number whose cube we calculated.
      // That way we can be super-sure that all permutations have already been encountered.
      val firstMatch: Option[Long] = (start until (start + newCubes.size / 3)).toList.find(i => {
          println(i)
          Integer.permutations(i * i * i).count(ip => allCubes.contains(ip)) == 5})

      if (firstMatch.isEmpty) SearchOutput(Some((start + newCubes.size / 3, allCubes)), None)
      else SearchOutput(None, Some(firstMatch.get))
    }
    StreamSearcher[Long, (Long, HashSet[Long]), Long](Integer.ints(1), searcher, (_) => false, 21).search.get.toString
  }
}

object P0062 extends App {
  (new P0062).printAnswer()
}
