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
}
