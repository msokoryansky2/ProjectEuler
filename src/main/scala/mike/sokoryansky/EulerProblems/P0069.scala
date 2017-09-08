package mike.sokoryansky.EulerProblems

import scala.collection.immutable.{HashSet, SortedSet}
import mike.sokoryansky.MathUtils.{Integer, Prime}

/*
Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of numbers
less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8,
are all less than nine and relatively prime to nine, φ(9)=6.

n 	Relatively Prime 	φ(n) 	n/φ(n)
2 	1 	              1 	  2
3 	1,2 	            2 	  1.5
4 	1,3 	            2 	  2
5 	1,2,3,4 	        4 	  1.25
6 	1,5 	            2 	  3
7 	1,2,3,4,5,6 	    6 	  1.1666...
8 	1,3,5,7 	        4 	  2
9 	1,2,4,5,7,8 	    6 	  1.5
10 	1,3,7,9 	        4 	  2.5

It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.

Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.
*/

class P0069 extends EulerProblem {
  val mil = 1000000L

  def run: String = Prime.totient2toN(mil).map(n => n._1 -> n._1.toDouble / n._2).maxBy(_._2)._1.toString

  def run2: String = {
    val primes = Prime.primes(1).takeWhile(_ <= Math.sqrt(mil).ceil.toLong).toList.sorted
    val factors: Map[Long, Map[Long, Long]] = (2L to mil).map(n => n -> Prime.primeFactorsWithLookup(n, primes)).toMap
    (2L to mil).map(n => n -> n.toDouble / Prime.totient(n, factors(n))).toMap.maxBy(_._2)._1.toString
  }
}

object P0069 extends App {
  (new P0069).printAnswer()
}
