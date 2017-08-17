package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Integer, Prime}

import scala.annotation.tailrec

class P0047 extends EulerProblem {
  def run: String = {
    @tailrec def tryChunk(chunkSize: Int, chunkNumber: Int, knownPrimes: Set[Long]): Long = {
      val start = Math.max(1, chunkNumber * chunkSize - 5)
      val end = (chunkNumber + 1) * chunkSize
      val primes = knownPrimes union (start.toLong to (end / 2 + 3)).filter(Prime.isPrime).toSet
      val matches = (start to end).filter (i => {
            val i0 = i
            val i1 = i + 1
            val i2 = i + 2
            val i3 = i + 3
            primes.count(i0 % _ == 0) == 4 &&
            primes.count(i1 % _ == 0) == 4 &&
            primes.count(i2 % _ == 0) == 4 &&
            primes.count(i3 % _ == 0) == 4
      })
      if (matches.isEmpty) tryChunk(chunkSize, chunkNumber + 1, primes) else matches.head
    }
    tryChunk(10000, 0, Set()).toString
  }
}

object P0047 extends App {
  (new P0047).printAnswer()
}
