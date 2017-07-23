package com.msokoryansky.EulerProblems

import scala.annotation.tailrec

class P7 extends EulerProblem {
  /**
    * @param i first integer in the stream
    * @return Stream of all integers starting with i
    */
  def ints(i: Int): Stream[Int] = i #:: ints(i + 1)

  def isPrime(n: Int): Boolean = {
    n match {
      case x if x < 2 => false
      case _ => !((2 to Math.sqrt(n.toDouble).ceil.toInt).exists{x => x != n && n % x == 0})
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
    val p3 = new P3
    p3.primes(p3.ints(2)).drop(Math.max(0, n - 1)).head
  }

  def run = primeNumber(10001).toString
}
