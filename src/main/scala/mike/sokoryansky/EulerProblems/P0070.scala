package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Integer, Prime, Subset}

/*

Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of
positive numbers less than or equal to n which are relatively prime to n.
For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.

 */

/*
  Some brainstorming on how to cull down the problem size...
  1. Primes can be eliminated because totient of a prime is that prime minus 1, which is def not a permutation of prime
  2. Numbers whose totients have fewer digits than the number can be eliminated
      a. That means numbers that come soon after new power of 10 is reached. E.g. 1001, 100001, etc.
  3. We are looking for minimum n/φ(n) which would happen for large φ(n) which should happen for numbers that:
      a. ... have few factors (but do have some -- we know they can't be prime numbers from (1) above)
      b. ... have large factors, so those factors have relatively few divisors
     Therefore one strategy may be to iterate through multiples of primes starting with fewest/largest primes.
 */


class P0070 extends EulerProblem {
  val tenToSeventh = 10000000

  def run: String = {
    // Try combinations of primes ordered by their product as (perfect or imperfect?) proxy for sorting by min n/φ(n)
    def primeProductsWalk(numPrimes: Int): List[(Set[Long], Long, Long)] = {
      val primes = Prime.primes(1).takeWhile(_ <= Math.pow(tenToSeventh, 1.0 / numPrimes).ceil.toLong).toSet
      val primesSets = Subset.subsetsDupes(primes, numPrimes).toList.sortBy(_.product)
      val primesSetsMatches = primesSets.map(set => (set, set.product, set.map(p => p - 1).product))
                                .filter(setInfo => Integer.isPermutation(setInfo._2, setInfo._3))
      println(primesSetsMatches)
      if (primesSetsMatches.nonEmpty) primesSetsMatches else primeProductsWalk(numPrimes + 1)
    }
    primeProductsWalk(2).minBy(setInfo => setInfo._2.toDouble / setInfo._3).toString
  }

  def run2: String = Prime.totient1toN(tenToSeventh - 1)
      .filter(ntn => ntn._1 != 1 && Integer.isPermutation(ntn._1, ntn._2))
      .map(ntn => ntn._1 -> ntn._1.toDouble / ntn._2)
      .minBy(_._2)._1.toString
}

object P0070 extends App {
  (new P0070).printAnswer()
}
