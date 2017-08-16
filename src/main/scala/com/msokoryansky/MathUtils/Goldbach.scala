package com.msokoryansky.MathUtils

object Goldbach {
  /**
    * It was proposed by Christian Goldbach that every odd composite number
    * can be written as the sum of a prime and twice a square.
    * 9 = 7 + 2×12
    * 15 = 7 + 2×22
    * 21 = 3 + 2×32
    * 25 = 7 + 2×32
    * 27 = 19 + 2×22
    * 33 = 31 + 2×12
    * It turns out that the conjecture was false.
    *
    * Attempt to find a Goldbach factor for an odd composite (non-prime) number and, if found,
    * return a tuple representing the prime number and number to be squared and multiplied by 2 to make up the original
    * number. Returns None if impossible to do such goldbach "factorization"
    *
    */
  def goldbachFactor(n: Long): Option[(Long, Long)] = {
    val maxSquare = Math.sqrt(n / 2).toLong
    def goldbackFactorWalk(square: Long): Option[(Long, Long)] = {
      if (square > maxSquare) None
      else {
        val pMaybe = n - 2 * square * square
        if (Prime.isPrime(pMaybe)) Some(square, pMaybe)
        else goldbackFactorWalk(square + 1)
      }
    }
    goldbackFactorWalk(1)
  }

  /**
    * Stream of "goldbach series" of odd compose (non-prime) numbers
    */
  def goldbachCandidates: Stream[Long] = Integer.ints(2.toLong).filter(n => n % 2 == 1 && !Prime.isPrime(n))
}
