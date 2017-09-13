package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{FareySeq, Fraction, Prime}

/*


Consider the fraction, n/d, where n and d are positive integers.
If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 3 fractions between 1/3 and 1/2.

How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?

 */

class P0073 extends EulerProblem {
  val twelvek = 12000
  def run: String = {
    val primeFactors = Prime.primes(1).takeWhile(_ <= Math.sqrt(twelvek).ceil.toLong).toList.sorted
    val denom2nums = (2 to twelvek).map(i => i -> {
      val lower = i / 3 + 1
      val upper = if (i % 2 == 0) i / 2 - 1 else i / 2
      //println(s"Range for $i : $lower to $upper with primes $primeFactors")
      (lower to upper).filter(n => !primeFactors.exists(p => n > p && n % p == 0))
    }).toMap.filterNot(_._2.isEmpty)
    denom2nums.map(_._2.size).sum.toString
  }
}

object P0073 extends App {
  (new P0073).printAnswer()
}
