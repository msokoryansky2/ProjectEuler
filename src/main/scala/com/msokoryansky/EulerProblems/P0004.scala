package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Integers

/*

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*/


class P0004 extends EulerProblem {
  def run: String = Integers.largestProductPalindrome(999, 100).toString
}

object P0004 extends App {
  (new P0004).printAnswer()
}