package com.msokoryansky.MathUtils

object Amicable {
  def isAmicable(i: Long, j: Long): Boolean = {
    if (i == j) false
    else if (Integer.divisors(i).filter(_ != i).sum != j) false
    else Integer.divisors(j).filter(_ != j).sum == i
  }
}
