package com.msokoryansky.MathUtils

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Integer {
  /**
    * @param i first integer in the stream
    * @return Stream of all integers starting with i
    */
  def ints(i: Int): Stream[Int] = i #:: ints(i + 1)
  def ints(i: BigInt): Stream[BigInt] = i #:: ints(i + 1)
  def ints(i: Long): Stream[Long] = i #:: ints(i + 1)

  def intsDesc(hi: BigInt, lo: BigInt): Stream[BigInt] = if (hi >= lo) hi #:: intsDesc(hi - 1, lo) else Stream.Empty

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

  def factorial(i: Int): Long = {
    require(i >= 0, "Can only take factorials of positive integers")
    @tailrec def factorialAcc(i: Int, acc: Long): Long = if (i <= 1) acc else factorialAcc(i - 1, i * acc)
    factorialAcc(i, 1)
  }

  /**
    * Returns greatest product of numDigits consecutive digits in the number represented by a String
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

  def isSumOf2Elements(number: Long, elements: Set[Long]): Boolean = {
    elements.exists(e1 => elements.contains(number - e1))
  }
}


