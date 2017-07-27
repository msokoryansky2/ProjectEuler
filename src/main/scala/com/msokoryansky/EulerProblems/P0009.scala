package com.msokoryansky.EulerProblems

/*

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*/


class P0009 extends EulerProblem {
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

  def run: String = pythagoreanProduct(pythagoreanTripletBySum(1000))
}

object P0009 extends App {
  (new P0009).printAnswer()
}