package com.msokoryansky.EulerProblems

import scala.annotation.tailrec


/*

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*/


class P0005 extends EulerProblem {
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
    val p3 = new P0003
    @tailrec def primeFactorsOfRangeAcc(lo: BigInt, hi: BigInt, acc: Seq[BigInt]): Seq[BigInt] = {
      if (lo > hi) acc
      else primeFactorsOfRangeAcc(lo + 1, hi, union2(acc, p3.primeFactors(lo, p3.primes(p3.ints(2)), Nil)))
    }
    primeFactorsOfRangeAcc(lo, hi, Seq.empty)
  }

  def run: String = primeFactorsOfRange(1, 20).product.toString()
}

object P0005 extends App {
  (new P0005).printAnswer()
}