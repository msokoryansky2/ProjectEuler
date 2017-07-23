package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP4 extends FunSuite {
  test("P4.isNumberPalindrome should detect palindromes") {
    val p4 = new P4
    assert(p4.isNumberPalindrome(0))
    assert(p4.isNumberPalindrome(7))
    assert(p4.isNumberPalindrome(1221))
    assert(p4.isNumberPalindrome(12344321))
    assert(!p4.isNumberPalindrome(253))
    assert(!p4.isNumberPalindrome(1234474321))
  }

  test("P4.intsDesc produces stream of descending integers") {
    val p4 = new P4
    assert(p4.intsDesc(12, 10).toList === List(12, 11, 10))
    assert(p4.intsDesc(2, 10).toList === List())
  }

  test("P4.largestProductPalindrome produces largest palindrome that's product of two numbers in integer range") {
    val p4 = new P4
    assert(p4.largestProductPalindrome(10, 1) === 9)
    assert(p4.largestProductPalindrome(12, 11) === 121)
    assert(p4.largestProductPalindrome(12, 0) === 121)
    assert(p4.largestProductPalindrome(4, 0) === 9)
  }
}