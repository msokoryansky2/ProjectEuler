package com.msokoryansky.MathUtils

import scala.annotation.tailrec

object Palindrome {
  def isNumberPalindrome(i: BigInt): Boolean = i.toString == i.toString.reverse

  def largestProductPalindrome(i: BigInt, j: BigInt): BigInt = {
    @tailrec def largestProductPalindromeAcc(outerStart: BigInt,
                                             outer: BigInt, inner: BigInt, largest: BigInt): BigInt = {
      if (outer < j) largest
      else if (inner < j || outer * inner <= largest)
        largestProductPalindromeAcc(outerStart - 1,outerStart - 1, i, largest)
      else if (outer * inner > largest && isNumberPalindrome(outer * inner))
        largestProductPalindromeAcc(outerStart - 1,outerStart - 1, i, outer * inner)
      else largestProductPalindromeAcc(outerStart, outer, inner - 1, largest)
    }
    largestProductPalindromeAcc(i, i, i, 0)
  }
}
