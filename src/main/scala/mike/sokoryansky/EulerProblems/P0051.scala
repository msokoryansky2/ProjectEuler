package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Integer, Prime, Subset}

import scala.annotation.tailrec


/*

By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values:
13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit,
this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family:
56003, 56113, 56333, 56443, 56663, 56773, and 56993.
Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit,
is part of an eight prime value family.

*/


class P0051 extends EulerProblem {
  def run: String = {
    @tailrec def tryNumDigits(numDigits: Int): Long = {
      val subset = Integer.subsetsWithFixedDigits(numDigits).filterNot(_.size >= numDigits)
                    .find(s => Prime.primesFromDigitSubstitution(numDigits, s).size >= 8)
      subset match {
        case Some(s) => Prime.primesFromDigitSubstitution(numDigits, s).min
        case None => tryNumDigits(numDigits + 1)
      }
    }
    tryNumDigits(1).toString
  }
}

object P0051 extends App {
  (new P0051).printAnswer()
}