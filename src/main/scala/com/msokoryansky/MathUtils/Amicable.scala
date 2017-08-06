package com.msokoryansky.MathUtils

object Amicable {
  def isAmicable(i: Long, j: Long): Boolean = i != j &&
      Integer.divisors(i).filter(_ != i).sum == j &&
      Integer.divisors(j).filter(_ != j).sum == i
}
