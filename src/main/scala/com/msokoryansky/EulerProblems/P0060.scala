package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Integer, Prime, SearchInput, SearchOutput, StreamSearcher, Subset}

import scala.collection.immutable.HashSet

/*

The primes 3, 7, 109, and 673, are quite remarkable.
By taking any two primes and concatenating them in any order the result will always be prime.
For example, taking 7 and 109, both 7109 and 1097 are prime.
The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

 */

class P0060 extends EulerProblem {

  def run: String = {
    StreamSearcher[Long, P0060State, Seq[Long]](Prime.primes(2.toLong), searcher, _ => false, 1).search.get.sum.toString
  }

  case class P0060State(primes: HashSet[String], concatenations: Map[String, HashSet[String]])

  object P0060State {
    def empty: P0060State = new P0060State(HashSet[String](), Map[String, HashSet[String]]())
  }

  def searcher(input: SearchInput[Long, P0060State]): SearchOutput[P0060State, Seq[Long]] = {
    val oldPrimes = input.state.getOrElse(P0060State.empty).primes
    val newPrime = input.elements.head.toString
    val allPrimes = oldPrimes ++ IndexedSeq(newPrime)

    val oldConcs: Map[String, HashSet[String]] = input.state.getOrElse(P0060State.empty).concatenations
    val newConcsAB: HashSet[String] = oldPrimes.filter(p => Prime.isPrime((p + newPrime).toLong))
    val newConcsBA: HashSet[String] = oldPrimes.filter(p => Prime.isPrime((newPrime + p).toLong))
    val allConcs: Map[String, HashSet[String]] = Map[String, HashSet[String]](newPrime -> newConcsAB) ++
      oldConcs.map(c => c._1 -> (c._2 ++ (if (newConcsBA.contains(c._1)) HashSet(newPrime) else Nil)))

    val candidates = allConcs.filter(_._2.size >= 3).keys.toList.sortWith(_ < _)

    val matches = for {
      a <- candidates
      b <- candidates.filter(_ > a)
      if allConcs(a).contains(b) && allConcs(b).contains(a)
      c <- candidates.filter(_ > b)
      if allConcs(a).contains(c) && allConcs(b).contains(c) && allConcs(c).contains(a) && allConcs(c).contains(b)
      d <- candidates.filter(_ > c)
      if allConcs(a).contains(d) && allConcs(b).contains(d) && allConcs(c).contains(d) &&
        allConcs(d).contains(a) && allConcs(d).contains(b) && allConcs(d).contains(c)
      e <- candidates.filter(_ > d)
      if allConcs(a).contains(e) && allConcs(b).contains(e) && allConcs(c).contains(e) && allConcs(d).contains(e) &&
          allConcs(e).contains(a) && allConcs(e).contains(b) && allConcs(e).contains(c) && allConcs(e).contains(d)
    } yield List(a, b, c, d, e).map(_.toLong)

    matches match {
      case emp if emp.isEmpty => SearchOutput(Some(new P0060State(allPrimes, allConcs)), None)
      case ms => SearchOutput(None, Some(ms.minBy(_.sum)))
    }
  }
}

object P0060 extends App {
  (new P0060).printAnswer()
}
