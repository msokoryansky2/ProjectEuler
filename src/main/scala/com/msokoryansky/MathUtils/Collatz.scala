package com.msokoryansky.MathUtils

import scala.annotation.tailrec

object Collatz {
  def collatz(i: Long): List[Long] = {
    require(i > 0, "Only applicable to positive integers")
    @tailrec def collatzAcc(acc: List[Long]): List[Long] =
      if (acc.head <= 1) acc
      else if (acc.head % 2 == 0) collatzAcc(acc.head / 2 :: acc)
      else collatzAcc(3 * acc.head + 1 :: acc)
    collatzAcc(List(i)).reverse
  }

  def longestCollatz(start: Long, stop: Long): Long = {
    @tailrec def longestCollatzAcc(start: Long, acc: List[Long]): List[Long] = {
      if (start >= stop) acc
      else {
        val thisCollatz = collatz(start)
        longestCollatzAcc(start + 1, if (thisCollatz.length > acc.length) thisCollatz else acc)
      }
    }
    longestCollatzAcc(start, List[Long]()).head
  }
}
