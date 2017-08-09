package com.msokoryansky.EulerProblems


/*
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits
1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*/


class P0024 extends EulerProblem {
  def run: String = {
    // Brute force solution.
    // An elegant algo would use the fact that we only need to find millionth element.
    // The general approach is to see what number should go first lexigraphically in order to be millionth.
    // Then recurse remaining characters with remaining number to get to million.
    val perms = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9").permutations.toArray.
      map(_.mkString).sortWith(_ < _)
    perms(999999)
  }
}

object P0024 extends App {
  (new P0024).printAnswer()
}
