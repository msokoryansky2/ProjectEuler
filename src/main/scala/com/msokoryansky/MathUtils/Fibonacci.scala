package com.msokoryansky.MathUtils

import scala.annotation.tailrec

object Fibonacci {
  /**
    * @param a first number in Fibonacci stream
    * @param b second number in Fibonacci stream
    * @return stream of Fibonacci numbers, starting with a, b
    */
  def fibs(a: Int, b: Int): Stream[Int] = a #:: fibs(b, a + b)

  def fibsWithIndex(a: HugePositiveInt, b: HugePositiveInt, index: Long): Stream[(HugePositiveInt, Long)] =
    (a, index) #:: fibsWithIndex(b, a + b, index + 1)

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
}
