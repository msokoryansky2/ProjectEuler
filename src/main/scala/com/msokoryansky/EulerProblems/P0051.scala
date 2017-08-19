package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Prime, Subset}


/*

By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values:
13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit,
this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family:
56003, 56113, 56333, 56443, 56663, 56773, and 56993.
Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit,
is part of an eight prime value family.



class P0051 extends EulerProblem {
  def run: String = {
    def tryNumDigits(n: Int): Option[Long] = {
      val fixedPlaces = Subset.subsets((0 to n).toSet).filterNot(_.isEmpty)
      val eightPrimePlaces = fixedPlaces.foreach(s => {
        val numFixedDigits = fixedPlaces.size
        val fixedDigitsCandidates =
          (("1" + "0" * numFixedDigits).toLong until ("1" + "0" * (1 + numFixedDigits)).toLong by 2)
            .map(digits => digits.toString.substring(1).toList.map(_.asDigit))
        // At this point s is e.g. Set(0, 2, 5) and fixedDigits is e.g. List(6, 7, 6)
        // We zip them up to form Map(0 -> 6, 2 -> 7, 5 -> 6)
        fixedDigitsCandidates.map(fixedDigits => Prime.primesFromDigitSubstitution(n, s.zip(fixedDigits).toMap))
      })
      ???
    }
  }
}

object P0051 extends App {
  (new P0051).printAnswer()
}

 */