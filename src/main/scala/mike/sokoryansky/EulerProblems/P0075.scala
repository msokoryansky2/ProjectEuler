package mike.sokoryansky.EulerProblems


import mike.sokoryansky.MathUtils.{Integer, Prime, Pythagorean}

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
    val prims = Pythagorean.pythagoreanTriplesPrimitive1ToN(perim).map(t => t._1 -> t._2.size)
    val (prims1, prims2) = prims.partition(_._2 == 1)
    val prims1Keys: HashSet[Long] =  HashSet[Long]() ++ prims1.keySet
    val prims2Keys: HashSet[Long] =  HashSet[Long]() ++ prims2.keySet
    // Start with candidates being all even numbers
    val candidates1: HashSet[Long] = HashSet[Long]() ++ (12L to perim by 2).toList.toSet
    println("1st candidate list size = " + candidates1.size)
    // Eliminate candidates for whom there're perimeters with multiple prims
    val candidates2 = candidates1.diff(prims2Keys)
    println("2nd candidate list size = " + candidates2.size)

    //println("Perims with 1 primitive triple = " + prims1.size)
    // List of all perimeters with one primitive Pyth triple AND not a multiple of another perim with Pyth triple
    //val primitives1Keys = prims1.keys
    //val ones = primitives1Keys.filterNot(p => primitives1Keys.exists(p2 => p > p2 && p % p2 == 0))
    //println("Perims with 1 primitive triple that are NOT multiple of other perims = " + ones.size)

    "N/A"
  }
}

object P0075 extends App {
  (new P0075).printAnswer()
}
