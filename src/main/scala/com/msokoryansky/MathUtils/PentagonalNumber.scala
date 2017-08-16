package com.msokoryansky.MathUtils

import scala.annotation.tailrec

object PentagonalNumber {
  /**
    * Returns nth pentagonal number given by formula pn = n(3n−1)/2
    */
  def pentagonalNumber(n: Int): Long = {
    require(n > 0, "Must specify positive index for pentagonal number")
    n.toLong * (3 * n - 1) / 2
  }

  /**
    * Returns whether specified number is a pentagonal one and its number in pentagonal series.
    * We derive this by solving for n given pn where
    * pn = n(3n−1)/2:
    * 3/2n*n - 1/2n = pn
    * 3*n*n - n = 2pn
    * 3*n*n - n - 2pn = 0
    * Using quadratic formula and only looking at positive solutions we get:
    * n = (1 + SQRT(1 + 24pn) / 6)
    * If n is an integer, we know this number is a pentagonal one
    */
  def getPentagonalNumberIndex(pn: Long): Option[Int] = {
    if (pn < 1) None
    else {
      val sqrt = Math.sqrt(1 + 24 * pn)
      if (!sqrt.isWhole() || (1 + sqrt.toInt) % 6 != 0) None
      else Some((1 + sqrt.toInt) / 6)
    }
  }

  /**
    * Checks is specified number is a triangle one
    */
  def isPentagonalNumber(pn: Long): Boolean = getPentagonalNumberIndex(pn).getOrElse(0) > 0

  /**
    * Returns stream of pentagonals starting with specified index
    */
  def pentagonalNumbers(n: Int = 1): Stream[Long] = pentagonalNumber(n) #:: pentagonalNumbers(n + 1)

  /**
    * Finds absolute value of difference between two pentagonal numbers, by their index in the pentagonal series
    */
  def distanceBetweenPentagonalNumbers(n: Int, m: Int): Long = Math.abs(pentagonalNumber(n) - pentagonalNumber(m))

  /**
    * Returns first pentagonal number index n such that Pn <= target.
    * We know that n * (3 * n - 1) / 2 ~ target, so we approximate n as solution of that quadratic equation, so
    * n ~ (1 + sqrt(1 + 24 * target)) / 2
    *
    * We guesstimate n by taking square root of target
    * and go from there
    */
  def precedingPentagonalNumber(target: Long): Int = {
    require(target > 0, "Must specify positive integer as approximate target")
    def precedingPentagonalNumberAcc(guess: Int): Int = {
      if (pentagonalNumber(guess) <= target && pentagonalNumber(guess + 1) >= target) guess
      else if (pentagonalNumber(guess) < target) precedingPentagonalNumberAcc(guess + 1)
      else precedingPentagonalNumberAcc(guess - 1)
    }
    precedingPentagonalNumberAcc(Math.max(1, (1 + Math.sqrt(1 + 24 * target)) / 2).toInt)
  }
  def succeedingPentagonalNumber(target: Long): Int = {
    require(target > 0, "Must specify positive integer as approximate target")
    def succeedingPentagonalNumberAcc(guess: Int): Int = {
      if (pentagonalNumber(guess) >= target && (guess == 1 || pentagonalNumber(guess - 1) < target)) guess
      else if (pentagonalNumber(guess) < target) succeedingPentagonalNumberAcc(guess + 1)
      else succeedingPentagonalNumberAcc(guess - 1)
    }
    succeedingPentagonalNumberAcc(Math.max(1, (1 + Math.sqrt(1 + 24 * target)) / 2).toInt)
  }

  /**
    * Returns index of first pentagonal number whose next neighbor is more than specified distance away.
    * Distance between 2 consecutive pentagonal numbers is (3 * n + 1), so n > (distance - 1) / 3
    */
  def firstRemotePentagonalNumber(distance: Long): Int = {
    val guess = ((distance - 1) / 3).toInt
    if (pentagonalNumber(guess + 1) - pentagonalNumber(guess) <= distance) guess + 1 else guess
  }

  /**
    * Exhaustive breadth-first search of pentagonal series for first pentagonal pair satisfying specified
    * property, beginning with index start
    */
  def firstPentagonalPairWithProperty(start: Int, p: (Int, Int) => Boolean): (Int, Int) = {
    require(start > 0, "Starting index must be positive")
    @tailrec def firstPentagonalPairWithPropertyWalk(current: Int, end: Int): (Int, Int) = {
      if (current == 0) firstPentagonalPairWithPropertyWalk(end, end + 1)
      else if (p(current, end)) (current, end)
      else firstPentagonalPairWithPropertyWalk(current - 1, end)
    }
    firstPentagonalPairWithPropertyWalk(start, start + 1)
  }

  /**
    * Exhaustive search of all pairs of pentagonals meeting specified property, between start and end index, inclusive
    */
  def allPentagonalPairsWithinDistanceWithProperty(start: Int, finish: Int, distance: Long,
                                                    p: (Int, Int) => Boolean): List[(Int, Int)] = {
    require(start > 0, "Starting index must be positive")
    require(finish > 0, "Finishing index must be positive")
    require(finish > start, "Starting index must be less than the finishing index")
    @tailrec def allPentagonalPairsWithPropertyAcc(current: Int, end: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      if (current < start || distanceBetweenPentagonalNumbers(current, end) > distance) {
        if (end >= finish) acc
        else allPentagonalPairsWithPropertyAcc(end , end + 1, acc)
      }
      else if (p(current, end)) allPentagonalPairsWithPropertyAcc(current - 1, end, (current, end) :: acc)
      else allPentagonalPairsWithPropertyAcc(current - 1, end, acc)
    }
    allPentagonalPairsWithPropertyAcc(start, start + 1,  List[(Int, Int)]())
  }

  /**
    * Checks if both sum and difference of two specified numbers are pentagonal numbers
    */
  def propertyPentagonalSumAndDiff(n1: Int, n2: Int): Boolean = {
    val pentagonalN1 = pentagonalNumber(n1)
    val pentagonalN2 = pentagonalNumber(n2)
    isPentagonalNumber(pentagonalN1 + pentagonalN2) && isPentagonalNumber(Math.abs(pentagonalN1 - pentagonalN2))
  }
}
