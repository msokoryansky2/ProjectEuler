package com.msokoryansky.EulerProblems

import scala.annotation.tailrec

class P0004 extends EulerProblem {
  def isNumberPalindrome(i: BigInt): Boolean = i.toString == i.toString.reverse

  def intsDesc(hi: BigInt, lo: BigInt): Stream[BigInt] = if (hi >= lo) hi #:: intsDesc(hi - 1, lo) else Stream.Empty

  def largestProductPalindrome(i: BigInt, j: BigInt): BigInt = {
    @tailrec def largestProductPalindromeAcc(outerStart: BigInt, outer: BigInt, inner: BigInt, largest: BigInt): BigInt = {
      if (outer < j) largest
      else if (inner < j || outer * inner <= largest) largestProductPalindromeAcc(outerStart - 1,outerStart - 1, i, largest)
      else if (outer * inner > largest && isNumberPalindrome(outer * inner)) largestProductPalindromeAcc(outerStart - 1,outerStart - 1, i, outer * inner)
      else largestProductPalindromeAcc(outerStart, outer, inner - 1, largest)
    }
    largestProductPalindromeAcc(i, i, i, 0)
  }

  def run: String = largestProductPalindrome(999, 100).toString
}

object P0004 extends App {
  (new P0004).printAnswer()
}