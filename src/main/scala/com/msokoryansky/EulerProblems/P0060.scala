package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Prime, SearchInput, SearchOutput, StreamSearcher}

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
    def acc(nextNumber: Long, primes: List[Long], concats: Map[Long, HashSet[Long]]): List[Long] = {
      val newPrime = Prime.nextPrime(nextNumber, primes)
      val allPrimes = newPrime :: primes
      val newConcatsAB: HashSet[Long] =
        HashSet[Long]() ++ primes.filter(p => Prime.isPrime2((p.toString + newPrime.toString).toLong, allPrimes))
      val newConcatsBA: List[Long] =
        primes.filter(p => Prime.isPrime2((newPrime.toString + p.toString).toLong, allPrimes))
      val newConcats: Map[Long, HashSet[Long]] =
        Map[Long, HashSet[Long]](newPrime -> newConcatsAB) ++
        concats.map(c => c._1 -> (c._2 ++ (if (newConcatsBA.contains(c._1)) HashSet(newPrime) else Nil)))

      val candidates: List[Long] = newConcats.filter(_._2.size >= 4).keys.toList.sortWith(_ < _)
      val candidatesJustUpdated: List[Long] = candidates.filter(newConcatsBA.contains)

      val matches: List[List[Long]] =
        for {
          b <- candidatesJustUpdated
          a = newPrime
          if newConcats(a).contains(b) && newConcats(b).contains(a)
          c <- candidates.filter(i => i != a && i != b)
          if newConcats(a).contains(c) && newConcats(b).contains(c) &&
            newConcats(c).contains(a) && newConcats(c).contains(b)
          d <- candidates.filter(i => i != a && i != b && i > c)
          if newConcats(a).contains(d) && newConcats(b).contains(d) && newConcats(c).contains(d) &&
            newConcats(d).contains(a) && newConcats(d).contains(b) && newConcats(d).contains(c)
          e <- candidates.filter(i => i != a && i != b && i > d)
          if newConcats(a).contains(e) && newConcats(b).contains(e) &&
            newConcats(c).contains(e) && newConcats(d).contains(e) &&
            newConcats(e).contains(a) && newConcats(e).contains(b) &&
            newConcats(e).contains(c) && newConcats(e).contains(d)
        } yield List[Long](a, b, c, d, e)

      matches match {
        case Nil => acc(newPrime + 1, allPrimes, newConcats)
        case ms => ms.minBy(_.sum)
      }
    }
    acc(2, List(), Map[Long, HashSet[Long]]()).sum.toString
  }
}

object P0060 extends App {
  (new P0060).printAnswer()
}
