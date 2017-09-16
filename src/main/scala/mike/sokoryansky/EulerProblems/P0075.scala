package mike.sokoryansky.EulerProblems


import mike.sokoryansky.MathUtils.Integer

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/*

It turns out that 12 cm is the smallest length of wire that can be bent
to form an integer sided right angle triangle in exactly one way, but there are many more examples.

12 cm: (3,4,5)
24 cm: (6,8,10)
30 cm: (5,12,13)
36 cm: (9,12,15)
40 cm: (8,15,17)
48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle,
and other lengths allow more than one solution to be found; for example,
using 120 cm it is possible to form exactly three different integer sided right angle triangles.

120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L ≤ 1,500,000
can exactly one integer sided right angle triangle be formed?

 */

class P0075 extends EulerProblem {
  val perim = 1500000
  def run: String = {
    @tailrec def rightTriAcc(per: Long, matches: List[(Long, Long, Long)], acc: List[Long]): List[Long] = {
      if (per % 10000 == 0) println(per + "...")
      if (per > perim) acc
      else {
        val mults = matches.count(l2 => per % (l2._1 + l2._2 + l2._3) == 0)
        if (mults > 1) rightTriAcc(per + 2, matches, acc)
        else {
          val newTris = (for {
            a <- 1L to per / 4
            bNum = per * per - 2 * per * a
            bDenom = 2 * per - 2 * a
            if bNum % bDenom == 0
            b = bNum / bDenom
            if !matches.exists(m => a % m._1 == 0 && b % m._2 == 0 && a / m._1 == b / m._2)
            c = per - a - b
            if c * c == a * a + b * b
          } yield (a, b, c)).toList
          if (mults + newTris.size == 1) rightTriAcc(per + 2, newTris ++ matches, per :: acc)
          else rightTriAcc(per + 2, matches, acc)
        }
      }
    }
    rightTriAcc(4, List(), List()).size.toString
  }

  def run2: String = {
    // map from integers to their squares for all integers <= maxSideLength
    // (maxSideLength is max plausible length of any one side in a triangle with perimeter <= maxSideLength * 2)
    // def squares: Map[Long, Long] = (1L to maxSideLength).map(i => i -> i * i).toMap
    // reverse map from squares of integers to those integers <= maxSideLength
    // def squaresReverse: Map[Long, Long] = squares.map(s => s._2 -> s._1)
    @tailrec def rightTriAcc(per: Int, matches: List[(Int, Int, Int)], acc: List[Int]): List[Int] = {
      if (per % 100 == 0) println(per + "...")
      if (per > perim) acc
      else {
        val mults = matches.count(l2 => per % (l2._1 + l2._2 + l2._3) == 0)
        if (mults > 1) rightTriAcc(per + 2, matches, acc)
        else {
          val newTris = (for {
            a <- 1 to per / 4
            b <- (a + 1) to per / 2
            if !matches.exists(m => a % m._1 == 0 && b % m._2 == 0 && a / m._1 == b / m._2)
            c = per - a - b
            if c * c == a * a + b * b
          } yield (a, b, c)).toList
          if (newTris.nonEmpty) println(s"$per :" + newTris)
          if (mults + newTris.size == 1) rightTriAcc(per + 2, newTris ++ matches, per :: acc)
          else rightTriAcc(per + 2, newTris ++ matches, acc)
        }
      }
    }
    rightTriAcc(4, List(), List()).size.toString
  }
}

object P0075 extends App {
  (new P0075).printAnswer()
}
