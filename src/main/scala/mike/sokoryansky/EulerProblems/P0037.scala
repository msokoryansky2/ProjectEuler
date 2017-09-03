package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Integer, Prime}

/*

The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits
from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left:
3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

 */

class P0037 extends EulerProblem {
  def run: String = {
    def runAcc(i: Long, acc: List[Long]): List[Long] = {
      if (acc.length >= 11) acc
      else {
        val trims = (Integer.trimsLeft(i) ++ Integer.trimsRight(i)).distinct
        runAcc(i + 1, if (trims.exists(!Prime.isPrime(_))) acc else i :: acc)
      }
    }
    runAcc(11, List[Long]()).sum.toString
  }
}

object P0037 extends App {
  (new P0037).printAnswer()
}
