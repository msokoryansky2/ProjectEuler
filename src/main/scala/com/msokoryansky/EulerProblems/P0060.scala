package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{SearchInput, SearchOutput, StreamSearcher, Integer, Prime}

/*

The primes 3, 7, 109, and 673, are quite remarkable.
By taking any two primes and concatenating them in any order the result will always be prime.
For example, taking 7 and 109, both 7109 and 1097 are prime.
The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

 */

class P0060 extends EulerProblem {

  def run: String = {
    StreamSearcher[Long, P0060State, Seq[Long]](Prime.primes(2.toLong), searcher, (_) => false, 100).search.toString
  }

  case class P0060State(primes: IndexedSeq[Long], decatenations: Map[(Long, Long), Long])

  object P0060State {
    def empty: P0060State = new P0060State(IndexedSeq[Long](), Map[(Long, Long), Long]())
  }

  def searcher(input: SearchInput[Long, P0060State]): SearchOutput[P0060State, Seq[Long]] = {
    val newPrimes: IndexedSeq[Long] = input.elements.toIndexedSeq
    val allPrimes: IndexedSeq[Long] = newPrimes ++ input.state.getOrElse(P0060State.empty).primes
    val newDecs: Map[(Long, Long), Long] = newPrimes.toList.flatMap(n =>
                                              Integer.decatenate(n, allPrimes).map(i => i -> n)).toMap
    val allDecs: Map[(Long, Long), Long] = newDecs ++ input.state.getOrElse(P0060State.empty).decatenations

    val matches = for {
      a <- allPrimes
      b <- allPrimes.filter(_ > a)
      if allDecs.contains((a, b)) && allDecs.contains((a, b))
      c <- allPrimes.filter(_ > b)
      if allDecs.contains((a, b)) && allDecs.contains((b, c)) &&
        allDecs.contains((c, a)) && allDecs.contains((c, b))
      d <- allPrimes.filter(_ > c)
      if allDecs.contains((a, d)) && allDecs.contains((b, d)) && allDecs.contains((c, d)) &&
        allDecs.contains((d, a)) && allDecs.contains((d, b)) && allDecs.contains((d, c))
      //e <- allPrimes.filter(_ > c)
      //if allDecs.contains((a, e)) && allDecs.contains((b, e)) && allDecs.contains((c, e)) && allDecs.contains((d, e))
    } yield List(a, b, c, d, a)

    matches match {
      case emp if emp.isEmpty => SearchOutput(Some(new P0060State(allPrimes, allDecs)), None)
      case ms => SearchOutput(None, Some(ms.minBy(_.sum)))
    }
  }
}

object P0060 extends App {
  (new P0060).printAnswer()
}
