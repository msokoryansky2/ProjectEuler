package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPalindrome extends FunSuite {

  test("isNumberPalindrome should detect palindromes") {
    assert(Palindrome.isNumberPalindrome(0))
    assert(Palindrome.isNumberPalindrome(7))
    assert(Palindrome.isNumberPalindrome(1221))
    assert(Palindrome.isNumberPalindrome(12344321))
    assert(!Palindrome.isNumberPalindrome(253))
    assert(!Palindrome.isNumberPalindrome(1234474321))
  }

  test("largestProductPalindrome produces largest palindrome that's product of two numbers in integer range") {
    assert(Palindrome.largestProductPalindrome(10, 1) === 9)
    assert(Palindrome.largestProductPalindrome(12, 11) === 121)
    assert(Palindrome.largestProductPalindrome(12, 0) === 121)
    assert(Palindrome.largestProductPalindrome(4, 0) === 9)
  }
}
