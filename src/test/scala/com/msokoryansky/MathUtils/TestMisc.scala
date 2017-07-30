package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestMisc extends FunSuite {
  test("dropFirstMatch drops first occurrence of element from sequence") {
    assert(Misc.dropFirstMatch(List("blah", "bleh", "blah"), "blah") === List("bleh", "blah"))
    assert(Misc.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 6) === Array(7, 8, 7, 6, 0, 1, 6, 8))
    assert(Misc.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 16) === Array(6, 7, 8, 7, 6, 0, 1, 6, 8))
    assert(Misc.dropFirstMatch(Array[Int](), 6) === Array[Int]())
    assert(Misc.dropFirstMatch(Array(10), 10) === Array())
    assert(Misc.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 0) === Array(6, 7, 8, 7, 6, 1, 6, 8))
  }

  test("union2 drops creates union of two sequences with possible repeating elements") {
    assert(Misc.union2(List("blah", "bleh", "blah"), List("blah")).sortWith(_ > _)
      === List("blah", "bleh", "blah").sortWith(_ > _))
    assert(Misc.union2(Array(6, 6, 8), Array(6, 7, 6, 6, 9)).sortWith(_ > _)
      === Array(6, 6, 8, 6, 7, 9).sortWith(_ > _))
    assert(Misc.union2(Array(6, 6, 8), Array[Int]()).sortWith(_ > _) === Array(6, 6, 8).sortWith(_ > _))
    assert(Misc.union2(Array[BigInt](), Array[BigInt]()).sortWith(_ > _) === Array[BigInt]().sortWith(_ > _))
  }
}
