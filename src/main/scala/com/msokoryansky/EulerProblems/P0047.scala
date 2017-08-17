package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Integer, Prime}

import scala.annotation.tailrec

class P0047 extends EulerProblem {
  def run: String = {
    @tailrec def tryChunk(chunkSize: Int, chunkNumber: Int): Long = {
      val start = Math.max(1, chunkNumber * chunkSize - 5)
      val end = (chunkNumber + 1) * chunkSize
      val primes = Prime.primes2(Integer.ints(2.toLong)).takeWhile(_ < end / 2 + 3)
      val matches = (start to end).filter(i =>
                          primes.count((i + 0) % _ == 0) == 4 &&
                          primes.count((i + 1) % _ == 0) == 4 &&
                          primes.count((i + 2) % _ == 0) == 4 &&
                          primes.count((i + 3) % _ == 0) == 4)
      if (matches.isEmpty) tryChunk(chunkSize, chunkNumber + 1) else matches.head
    }
    tryChunk(10000, 0).toString
  }
}

object P0047 extends App {
  (new P0047).printAnswer()
}
