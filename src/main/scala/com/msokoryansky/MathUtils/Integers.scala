package com.msokoryansky.MathUtils

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Integers {
  /**
    * @param i first integer in the stream
    * @return Stream of all integers starting with i
    */
  def ints(i: Int): Stream[Int] = i #:: ints(i + 1)

  /**
    * @param i first integer in the stream
    * @return stream of all integers starting with i
    */
  def ints(i: BigInt): Stream[BigInt] = i #:: ints(i + 1)

  /**
    * @param i first integer to consider
    * @param limit cutoff at or above which we stop
    * @param include function to decide which integers should be included in the sum
    * @return Sum of all integers starting from i and less than limit that meet the include condition
    */
  def intsSum(i: Int, limit: Int, include: Int => Boolean): Int = {
    @tailrec def intsSumAcc(ints: Stream[Int], sum: Int): Int = {
      if (ints.head >= limit) sum
      else intsSumAcc(ints.tail, sum + (if (include(ints.head)) ints.head else 0))
    }
    intsSumAcc(ints(i), 0)
  }

  /**
    * @param a first number in Fibonacci stream
    * @param b second number in Fibonacci stream
    * @return stream of Fibonacci numbers, starting with a, b
    */
  def fibs(a: Int, b: Int): Stream[Int] = a #:: fibs(b, a + b)

  /**
    * Sum of all Fibonacci numbers starting with a and b and not exceeding limit and subject to include
    * @param a Int first Fib number
    * @param b Int second Fib number
    * @param limit cutoff at or above which we stop adding new Fib numbers
    * @param include function that decides whether any given Fib number should be included in the sum
    * @return Int sum of specified Fib numbers
    */
  def fibsSum(a: Int, b: Int, limit: Int, include: Int => Boolean): Int = {
    @tailrec def fibsSumAcc(fibStream: Stream[Int], sum: Int): Int = {
      if (fibStream.head >= limit) sum
      else fibsSumAcc(fibStream.tail, sum + (if (include (fibStream.head)) fibStream.head else 0))
    }
    fibsSumAcc(fibs(a, b), 0)
  }

  /**
    * Generates stream of prime numbers
    * @param ints stream of interegers
    * @return stream of prime numbers
    */
  def primes(ints: Stream[BigInt]): Stream[BigInt] = {
    ints.head #:: primes(ints.tail.filter{_ % ints.head != 0})
  }

  /**
    * Finds list of all prime factors of a number. A prime factor may repeat multiple times.
    * E.g. For 24 the answer is List(2, 2, 2, 3).
    * @param i number to factor
    * @param primesToCheck stream of primes
    * @param factors accumulator list of factors
    * @return complete list of all prime factors of a number so that product of all numbers in the list == i
    */
  @tailrec final def primeFactors(i: BigInt, primesToCheck: Stream[BigInt], factors: List[BigInt]): List[BigInt] = {
    if (i < 2) factors
    else if (i % primesToCheck.head == 0)
      primeFactors(i / primesToCheck.head, primes(ints(2)), primesToCheck.head :: factors)
    else primeFactors(i, primesToCheck.tail, factors)
  }

  def isNumberPalindrome(i: BigInt): Boolean = i.toString == i.toString.reverse

  def intsDesc(hi: BigInt, lo: BigInt): Stream[BigInt] = if (hi >= lo) hi #:: intsDesc(hi - 1, lo) else Stream.Empty

  def largestProductPalindrome(i: BigInt, j: BigInt): BigInt = {
    @tailrec def largestProductPalindromeAcc(outerStart: BigInt,
                                             outer: BigInt, inner: BigInt, largest: BigInt): BigInt = {
      if (outer < j) largest
      else if (inner < j || outer * inner <= largest)
        largestProductPalindromeAcc(outerStart - 1,outerStart - 1, i, largest)
      else if (outer * inner > largest && isNumberPalindrome(outer * inner))
        largestProductPalindromeAcc(outerStart - 1,outerStart - 1, i, outer * inner)
      else largestProductPalindromeAcc(outerStart, outer, inner - 1, largest)
    }
    largestProductPalindromeAcc(i, i, i, 0)
  }

  def dropFirstMatch[A](ls: Seq[A], value: A): Seq[A] = {
    val index = ls.indexOf(value)
    if (index < 0) ls
    else if (index == 0) ls.tail
    else {
      val (a, b) = ls.splitAt(index)
      a ++ b.tail
    }
  }

  def union2[A](a: Seq[A], b: Seq[A]): Seq[A] = {
    @tailrec def union2Acc(a: Seq[A], b: Seq[A], acc: Seq[A]): Seq[A] = {
      if (a.isEmpty && b.isEmpty) acc
      else if (a.isEmpty) union2Acc(b, Seq.empty, acc)
      else union2Acc(a.tail, dropFirstMatch(b, a.head), acc :+ a.head)
    }
    union2Acc(a, b, Seq.empty)
  }

  def primeFactorsOfRange(lo: BigInt, hi: BigInt): Seq[BigInt] = {
    @tailrec def primeFactorsOfRangeAcc(lo: BigInt, hi: BigInt, acc: Seq[BigInt]): Seq[BigInt] = {
      if (lo > hi) acc
      else primeFactorsOfRangeAcc(lo + 1, hi, union2(acc, primeFactors(lo, primes(ints(2)), Nil)))
    }
    primeFactorsOfRangeAcc(lo, hi, Seq.empty)
  }

  def isPrime(n: Int): Boolean = {
    n match {
      case x if x < 2 => false
      case _ => !(2 to Math.sqrt(n.toDouble).ceil.toInt).exists{x => x != n && n % x == 0}
    }
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

  def primeNumberNonTailRec(n: Int): BigInt = {
    Integers.primes(Integers.ints(2)).drop(Math.max(0, n - 1)).head
  }

  /**
    * Returns greatest product of numDigits consecutive digits in the number represented by a String
    *
    * @param number    a string representation of a arbitrarily large number
    * @param numDigits number of consecutive digits to consider for a product
    * @return greatest possible product of consecutive digits
    */
  def greatestProduct(number: String, numDigits: Int): Long = {
    @tailrec def greatestProductAcc(remaining: String, acc: Long): Long = {
      if (numDigits < 1 || remaining.length < numDigits) acc
      else greatestProductAcc(remaining.substring(1),
        Math.max(remaining.substring(0, numDigits).toList.map(_.toString.toLong).product, acc))
    }
    greatestProductAcc(number, 0)
  }

  def pythagoreanC(a: Int, b: Int, abcSum: Int): Option[Int] = {
    val cPossible = abcSum - (a + b)
    if (cPossible > 0 && cPossible * cPossible == a * a + b * b) Some(cPossible) else None
  }

  def pythagoreanTripletBySum(sum: Int): Option[(Int, Int, Int)] = {
    {for {
      a <- 1 until sum if sum > 1
      b <- 1 until sum if sum > 1
      c <- pythagoreanC(a, b, sum)
    } yield (a, b, c)}.headOption
  }

  def pythagoreanProduct(opt: Option[(Int, Int, Int)]): String = opt match {
    case Some((a, b, c)) => (a * b * c).toString
    case _ => "None"
  }

  /**
    * @param i first integer in the stream
    * @return Stream of all integers starting with i
    */
  def ints(i: Long): Stream[Long] = i #:: ints(i + 1)

  /**
    * Test if number is a prime (compares to all 1..SQRT(n))
    * @param n mumber to check if it's prime
    * @return if number is prime then true, else false
    */
  def isPrime(n: Long): Boolean = {
    n match {
      case x if x < 2 => false
      case _ => !(2 to Math.sqrt(n.toDouble).ceil.toInt).exists{x => x != n && n % x == 0}
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

  def divisors(number: Long): HashSet[Long] = {
    require(number > 0, "Must specify a positive integer")
    @tailrec def divisorsAcc(next: Long, ceiling: Long, acc: HashSet[Long]): HashSet[Long] = {
      if (next > ceiling) acc
      else {
        if (number % next > 0) divisorsAcc(next + 1, ceiling, acc)
        else {
          val factor1 = next
          val factor2 = number / next
          val acc1 = if (acc.contains(factor1)) acc else acc + factor1
          val acc2 = if (acc1.contains(factor2)) acc1 else acc1 + factor2
          divisorsAcc(factor1 + 1, factor2, acc2)
        }
      }
    }
    divisorsAcc(1, number, HashSet[Long]())
  }

  def triangleNumbers(lastTriangleIndex: Long, lastTriangleNumber: Long): Stream[Long] = {
    val nextTriangleNumber = lastTriangleNumber + lastTriangleIndex + 1
    nextTriangleNumber #:: triangleNumbers(lastTriangleIndex + 1, nextTriangleNumber)
  }
}


