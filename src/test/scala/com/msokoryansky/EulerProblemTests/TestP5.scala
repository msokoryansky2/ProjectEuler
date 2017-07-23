package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP5 extends FunSuite {
  test("P5.dropFirstMatch drops first occurrence of element from sequence") {
    val p5 = new P5
    assert(p5.dropFirstMatch(List("blah", "bleh", "blah"), "blah") === List("bleh", "blah"))
    assert(p5.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 6) === Array(7, 8, 7, 6, 0, 1, 6, 8))
    assert(p5.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 16) === Array(6, 7, 8, 7, 6, 0, 1, 6, 8))
    assert(p5.dropFirstMatch(Array[Int](), 6) === Array[Int]())
    assert(p5.dropFirstMatch(Array(10), 10) === Array())
    assert(p5.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 0) === Array(6, 7, 8, 7, 6, 1, 6, 8))
  }

  test("P5.union2 drops creates union of two sequences with possible repeating elements") {
    val p5 = new P5
    assert(p5.union2(List("blah", "bleh", "blah"), List("blah")).sortWith(_ > _) === List("blah", "bleh", "blah").sortWith(_ > _))
    assert(p5.union2(Array(6, 6, 8), Array(6, 7, 6, 6, 9)).sortWith(_ > _) === Array(6, 6, 8, 6, 7, 9).sortWith(_ > _))
    assert(p5.union2(Array(6, 6, 8), Array[Int]()).sortWith(_ > _) === Array(6, 6, 8).sortWith(_ > _))
    assert(p5.union2(Array[BigInt](), Array[BigInt]()).sortWith(_ > _) === Array[BigInt]().sortWith(_ > _))
  }

  test("P5.primeFactorsOfRange returns sequence of prime factors that jointly factor any number in specified range ") {
    val p5 = new P5
    assert(p5.primeFactorsOfRange(1, 4).sortWith(_ > _) === Seq(2, 2, 3).sortWith(_ > _))
    assert(p5.primeFactorsOfRange(2, 8).sortWith(_ > _) === Seq(2, 3, 2, 5, 7, 2).sortWith(_ > _))
  }
}