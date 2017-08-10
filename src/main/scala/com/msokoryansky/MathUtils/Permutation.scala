package com.msokoryansky.MathUtils

import scala.annotation.tailrec

object Permutation {
  /**
    * Returns ith element (in alphabetical order) of lexicographic permutations of chars in set s
    * @param s set of characters to be permutated
    * @param i lexigraphic number of permutation to return
    * @return ith lexigraphic permutation
    */
  def lexicographicPermutation(s: Set[Char], i: Int): String = {
    /*
    The general idea is to use the fact that we know that because the order of permutations is alphabetical,
    the first (N-1)! permutations will start with first (alphabetically) element. Next (N-1)! will start with
    second element, etc.
     */
    @tailrec def lexicographicPermutationAcc(s: Set[Char], i: Int, acc: String): String = {
      require(i >= 0, "Can only return elements startign with number 0 in lexicographic sequence")
      require(i < Integer.factorial(s.size), "Provided set has fewer permutations than the permutation number requested")
      s.size match {
        case 0 => acc
        case n =>
          val partition = Integer.factorial(n - 1)
          val headElementIndex = (i / partition).toInt
          val headElement = s.toArray.sortWith(_ < _)(headElementIndex)
          val tailIndex = (i % partition).toInt
          val tailSet = s.filter(_ != headElement)
          lexicographicPermutationAcc(tailSet, tailIndex, acc + headElement)
      }
    }
    lexicographicPermutationAcc(s, i, "")
  }
}
