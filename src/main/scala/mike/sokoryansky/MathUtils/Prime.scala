package mike.sokoryansky.MathUtils

import mike.sokoryansky.MathUtils.Integer.ints
import mike.sokoryansky.MathUtils.Misc.union2

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Prime {
  /**
    * Generates stream of prime numbers
    * @param ints stream of interegers
    * @return stream of prime numbers
    */
  def primes(ints: Stream[BigInt]): Stream[BigInt] = {
    ints.head #:: primes(ints.tail.filter{_ % ints.head != 0})
  }
  def primes(i: Long): Stream[Long] = {
    val np = nextPrime(i)
    np #:: primes(np + 1)
  }

  /**
    * Finds list of all prime factors of a number. A prime factor may repeat multiple times.
    * E.g. For 24 the answer is List(2, 2, 2, 3).
    * @param i number to factor
    * @param primesToCheck stream of primes
    * @param factors accumulator list of factors
    * @return complete list of all prime factors of a number so that product of all numbers in the list == i
    */
  @tailrec def primeFactors(i: BigInt, primesToCheck: Stream[BigInt], factors: List[BigInt]): List[BigInt] = {
    if (i < 2) factors
    else if (i % primesToCheck.head == 0)
      primeFactors(i / primesToCheck.head, primes(ints(2)), primesToCheck.head :: factors)
    else primeFactors(i, primesToCheck.tail, factors)
  }
  def primeFactorsUnique(i: BigInt): List[BigInt] = primeFactors(i, primes(ints(2)), List()).distinct


  /**
    * List of all prime factors (some may appear multiple times for specified number given list of all primes
    * that are less or equal to that number. Returns a map of prime numbers to how many times they occur. E.g.
    * primeFactors(24) = Map(2 -> 3, 3 -> 1)
    */
  def primeFactors(i: Long, primes: List[Long]): Map[Long, Long] = {
    require(i > 1, "Must specify integer larger than 1 to be factored")
    def primeFactorsAcc(i: Long, acc: Map[Long, Long]): Map[Long, Long] = {
      if (i == 1) acc
      else {
        val factor = primes.find(p => p <= i && i % p == 0).get
        primeFactorsAcc(i / factor, acc + (factor -> (acc.getOrElse(factor, 0L) + 1)))
      }
    }
    primeFactorsAcc(i, Map())
  }

  def primeFactorsOfRange(lo: BigInt, hi: BigInt): Seq[BigInt] = {
    @tailrec def primeFactorsOfRangeAcc(lo: BigInt, hi: BigInt, acc: Seq[BigInt]): Seq[BigInt] = {
      if (lo > hi) acc
      else primeFactorsOfRangeAcc(lo + 1, hi, union2(acc, primeFactors(lo, primes(ints(2)), Nil)))
    }
    primeFactorsOfRangeAcc(lo, hi, Seq.empty)
  }

  /**
    * Test if number is a prime (compares to all 1..SQRT(n))
    * @param n mumber to check if it's prime
    * @return if number is prime then true, else false
    */
  def isPrime(n: Long): Boolean = isPrime2(n, 2)
  def isPrime2(n: Long, firstIntegerToTest: Long): Boolean = {
    n match {
      case x if x < 2 => false
      case _ => !(firstIntegerToTest to Math.sqrt(n.toDouble).ceil.toInt).exists{x => x != n && n % x == 0}
    }
  }

  /**
    * Efficient prime-ness check if we have a guaranteed list of all prime numbers less than some number.
    * If the prime list goes to the sqrt of the number being checked, then that's all that's needed.
    * Otherwise we have to do usual one-by-one checking from the last prime on
    */
  def isPrime2(n: Long, earlyPrimes: List[Long]): Boolean = {
    val lastIntegerToTest = Math.sqrt(n.toDouble).ceil.toInt
    if (earlyPrimes.exists(i => i <= lastIntegerToTest && n % i == 0)) false
    else isPrime2(n, if (earlyPrimes.isEmpty) 2 else earlyPrimes.max)
  }

  def primeNumber(n: Int): BigInt = {
    if (n < 1) 2
    else {
      @tailrec def primeNumberAcc(ints: Stream[Int], acc: Int): BigInt = {
        if (!isPrime(ints.head)) primeNumberAcc(ints.tail, acc)
        else if (acc == n) ints.head
        else primeNumberAcc(ints.tail, acc + 1)
      }
      primeNumberAcc(ints(2), 1)
    }
  }

  /**
    * Next prime number that's greater than lowerLimit
    * @param lowerLimit lower cutoff (inclusive) for next prime
    * @return
    */
  def nextPrime(lowerLimit: Long): Long = {
    if (lowerLimit < 2) 2
    else {
      @tailrec def nextPrimeAcc(ints: Stream[Long]): Long = {
        if (isPrime(ints.head)) ints.head else nextPrimeAcc(ints.tail)
      }
      nextPrimeAcc(ints(lowerLimit))
    }
  }

  /**
    * If know all primes from 2 up to some number, then we can test for primeness more efficiently
    */
  def nextPrime(lowerLimit: Long, earlyPrimes: List[Long]): Long = {
    if (lowerLimit < 2) 2
    else {
      @tailrec def nextPrimeAcc(ints: Stream[Long]): Long = {
        if (isPrime2(ints.head, earlyPrimes)) ints.head else nextPrimeAcc(ints.tail)
      }
      nextPrimeAcc(ints(lowerLimit))
    }
  }

  /**
    * Returns sum of all primes that are less than limit
    * @param limit cutoff at which we stop summing primes
    * @return sum of all primes below cutoff
    */
  def primeNumberSum(limit: Int): Long = {
    @tailrec def primeNumberSumAcc(n: Long, acc: Long): Long = {
      val p = nextPrime(n + 1)
      if (p >= limit) acc else primeNumberSumAcc(p, acc + p)
    }
    primeNumberSumAcc(0, 0)
  }

  /**
    * Returns longest possible list of consecutive primes whose sum is under cutoff and is itself prime
    */
  def longestPrimeSumOfConsecutivePrimes(cutoff: Long): List[Long] = {
    val primes = Prime.primes(2).takeWhile(_ < cutoff).toList
    @tailrec def primeSumAcc(curr: List[Long], accCurr: List[Long], acc: List[Long]): List[Long] = {
      if (curr.isEmpty || curr.head + accCurr.sum >= cutoff) acc
      else primeSumAcc(curr.tail, curr.head :: accCurr,
          if (Prime.isPrime(curr.head + accCurr.sum) && accCurr.size >= acc.size) curr.head :: accCurr else acc)
    }
    primes.map(p => primeSumAcc(primes.dropWhile(_ < p), List(), List())).maxBy(_.size)
  }

  /**
    * Builds a number with numDigits digits, with some of its digits specified as fixed, via a map of digit # -> digit
    * The rest of the digits are flex and will be substituted with the same digit. So 10 new numbers will created.
    * Then the subset of those 10 will be returned.
    *
    * E.g. if numDigits is 5, and fixed is (0 -> 4, 3 -> 9) then we have base of 4xx9x. We will replace "x" with 0 to 9,
    * to form: 40090, 41191, 42292, 43393, 44494, 45595, 46696, 47797, 48898, 49999 and check each of those if it's
    * prime and return a List of those that are prime
    */
  def primesFromDigitSubstitution(numDigits: Int, fixed: Map[Int, Int]): Seq[Long] = {
    require(fixed.size < numDigits, "Must have at least one digit to be substituted")
    // Can't have 0 in 0th (leading digit) position
    require(!fixed.contains(0) || fixed(0) != 0, "Cannot have zero as leading digit")
    ((if (fixed.contains(0)) 0 else 1) to 9)
      .map(d => (0 until numDigits).map(dn => if (fixed.contains(dn)) fixed(dn) else d).mkString.toLong)
      .filter(Prime.isPrime)
  }


  /**
    * List of relative primes of n. For example, as 1, 2, 4, 5, 7, and 8 are relative primes of 9.
    * Providing a version that already has a map of all numbers from 1 to (at least) n to their prime factors
    * and also where this map needs to be built from scratch.
    *
    * We do not try to check the map for completeness -- if it's provided we assume it has mappings
    * from 2 to (at least) (n - 1). Exceptions will be thrown if it doesn't
    */
  def relativePrimes(n: Long): Seq[Long] =
    relativePrimes(n, (2.toLong to n).map(i => i -> (HashSet() ++ primeFactorsUnique(i).map(_.toLong))).toMap)
  def relativePrimes(n: Long, factors: Map[Long, HashSet[Long]]): Seq[Long] = {
    require(n > 1, "Can only find relative primes for integers larger than 1")
    Seq(1L) ++ (2L until n).filterNot(i => factors(i).exists(id => id != 1 && factors(n).contains(id)))
  }

  /**
    * List of relative primes from all numbers from 1 to n.
    * Optimized to be much faster than n equivalent calls to relativePrimes()
    */
  def relativePrimesThroughN(n: Long): Map[Long, Seq[Long]] =
    relativePrimesThroughN(n, (2.toLong to n).map(i => i -> (HashSet() ++ primeFactorsUnique(i).map(_.toLong))).toMap)
  def relativePrimesThroughN(n: Long, factors: Map[Long, HashSet[Long]]): Map[Long, Seq[Long]] = {
    require(n > 1, "Can only find relative primes for integers larger than 1")
    @tailrec def relativePrimesThroughNAcc(next: Long, acc: Map[Long, Seq[Long]]): Map[Long, Seq[Long]] = {
      if (next > n) acc
      // lack of non-trivial divisors means number is prime so it's relatively prime with all numbers below it
      else {
        val nextFacts = factors(next)
        if (nextFacts.isEmpty) relativePrimesThroughNAcc(next + 1, acc + (next -> (1L until next)))
        // for number with non-trivial divisors, relative primes are those numbers between 1 and it that are
        // relatively prime with all its divisors
        else {
          val relPrimes =
            (2L until next).filterNot(i => nextFacts.contains(i) || nextFacts.exists(d => i > d && !acc(i).contains(d)))
          relativePrimesThroughNAcc(next + 1, acc + (next -> (Seq(1L) ++ relPrimes)))

          // Dumb/slow but correct impl:
          // relativePrimesThroughNAcc(next + 1, acc + (next -> relativePrimes(next, divisors)))
        }
      }
    }
    relativePrimesThroughNAcc(2, Map[Long, Seq[Long]]())
  }

  /**
    * Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of numbers
    * less than n which are relatively prime to n.
    * For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
    *
    * As with relativePrimes(), we assume that if factor map is included it covers all numbers from 2 to (at least) n-1.
    */
  def totient(n: Long): Long = relativePrimes(n).size
  def totient(n: Long, factors: Map[Long, HashSet[Long]]): Long = relativePrimes(n, factors).size
  def totientThroughN(n: Long): Map[Long, Long] =
    relativePrimesThroughN(n).map(p => p._1 -> p._2.size.toLong)
  def totientThroughN(n: Long, factors: Map[Long, HashSet[Long]]): Map[Long, Long] =
    relativePrimesThroughN(n, factors).map(p => p._1 -> p._2.size.toLong)
}
