package com.msokoryansky.EulerProblems

/*

If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

 */

class P0039 extends EulerProblem {
  def run: String = {
    (for {
      p <- 1 to 1000
      a <- 1 until p
      b <- 1 until p
      if a <= b && (a + b ) < p
      c = p - (a + b)
      if c * c == a * a + b * b
    } yield p).groupBy(identity).mapValues(_.size).maxBy(_._2)._1.toString
  }
}

object P0039 extends App {
  (new P0039).printAnswer()
}
