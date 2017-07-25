package com.msokoryansky.EulerProblems

import scala.annotation.tailrec

class P9 extends EulerProblem {
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

object P9 extends App {
  (new P9).printAnswer()
}