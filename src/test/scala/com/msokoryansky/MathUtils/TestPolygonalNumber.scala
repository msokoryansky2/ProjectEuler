package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPolygonalNumber extends FunSuite {
  test("numbers is a stream of polygonal numbers") {
    intercept[Exception] {
      PolygonalNumber(-1).numbers(0)
    }
    intercept[Exception] {
      PolygonalNumber(0).numbers(0)
    }
    intercept[Exception] {
      PolygonalNumber(2).numbers(0)
    }
    intercept[Exception] {
      PolygonalNumber(3).numbers(0)
    }
    assert(PolygonalNumber(3).numbers(1).head === 1)
    assert(PolygonalNumber(3).numbers(1).tail.head === 3)
    assert(PolygonalNumber(3).numbers(1).tail.tail.head === 6)
    assert(PolygonalNumber(3).numbers(1).tail.tail.tail.head === 10)
    assert(PolygonalNumber(3).numbers(1).tail.tail.tail.tail.head === 15)

    assert(PolygonalNumber(4).numbers(1).head === 1)
    assert(PolygonalNumber(4).numbers(1).tail.head === 4)
    assert(PolygonalNumber(4).numbers(1).tail.tail.head === 9)
    assert(PolygonalNumber(4).numbers(1).tail.tail.tail.head === 16)
    assert(PolygonalNumber(4).numbers(1).tail.tail.tail.tail.head === 25)

    assert(PolygonalNumber(5).numbers(1).head === 1)
    assert(PolygonalNumber(5).numbers(1).tail.head === 5)
    assert(PolygonalNumber(5).numbers(1).tail.tail.head === 12)
    assert(PolygonalNumber(5).numbers(1).tail.tail.tail.head === 22)
    assert(PolygonalNumber(5).numbers(1).tail.tail.tail.tail.head === 35)

    assert(PolygonalNumber(6).numbers(1).head === 1)
    assert(PolygonalNumber(6).numbers(1).tail.head === 6)
    assert(PolygonalNumber(6).numbers(1).tail.tail.head === 15)
    assert(PolygonalNumber(6).numbers(1).tail.tail.tail.head === 28)
    assert(PolygonalNumber(6).numbers(1).tail.tail.tail.tail.head === 45)

    assert(PolygonalNumber(7).numbers(1).head === 1)
    assert(PolygonalNumber(7).numbers(1).tail.head === 7)
    assert(PolygonalNumber(7).numbers(1).tail.tail.head === 18)
    assert(PolygonalNumber(7).numbers(1).tail.tail.tail.head === 34)
    assert(PolygonalNumber(7).numbers(1).tail.tail.tail.tail.head === 55)

    assert(PolygonalNumber(8).numbers(1).head === 1)
    assert(PolygonalNumber(8).numbers(1).tail.head === 8)
    assert(PolygonalNumber(8).numbers(1).tail.tail.head === 21)
    assert(PolygonalNumber(8).numbers(1).tail.tail.tail.head === 40)
    assert(PolygonalNumber(8).numbers(1).tail.tail.tail.tail.head === 65)
  }

  test("indexOf and indexOfFractional returns Option of a number's index in its polygona series") {
    assert(PolygonalNumber(3).indexOf(-1) === None)
    assert(PolygonalNumber(3).indexOf(0) === None)
    assert(PolygonalNumber(3).indexOf(1).get === 1)
    assert(PolygonalNumber(3).indexOf(3).get === 2)
    assert(PolygonalNumber(3).indexOf(6).get === 3)
    assert(PolygonalNumber(3).indexOf(10).get === 4)
    assert(PolygonalNumber(3).indexOf(15).get === 5)
    assert(PolygonalNumber(3).indexOf(5) === None)
    assert(PolygonalNumber(3).indexOfFractional(5) > PolygonalNumber(3).indexOf(3).get)
    assert(PolygonalNumber(3).indexOfFractional(5) < PolygonalNumber(3).indexOf(6).get)
    assert(PolygonalNumber(3).indexOf(8) === None)
    assert(PolygonalNumber(3).indexOfFractional(8) > PolygonalNumber(3).indexOf(6).get)
    assert(PolygonalNumber(3).indexOfFractional(8) < PolygonalNumber(3).indexOf(10).get)

    assert(PolygonalNumber(4).indexOf(-1) === None)
    assert(PolygonalNumber(4).indexOf(0) === None)
    assert(PolygonalNumber(4).indexOf(1).get === 1)
    assert(PolygonalNumber(4).indexOf(4).get === 2)
    assert(PolygonalNumber(4).indexOf(9).get === 3)
    assert(PolygonalNumber(4).indexOf(16).get === 4)
    assert(PolygonalNumber(4).indexOf(25).get === 5)
    assert(PolygonalNumber(4).indexOf(5) === None)
    assert(PolygonalNumber(4).indexOfFractional(5) > PolygonalNumber(4).indexOf(4).get)
    assert(PolygonalNumber(4).indexOfFractional(5) < PolygonalNumber(4).indexOf(9).get)
    assert(PolygonalNumber(4).indexOf(12) === None)
    assert(PolygonalNumber(4).indexOfFractional(12) > PolygonalNumber(4).indexOf(9).get)
    assert(PolygonalNumber(4).indexOfFractional(12) < PolygonalNumber(4).indexOf(16).get)

    assert(PolygonalNumber(5).indexOf(-1) === None)
    assert(PolygonalNumber(5).indexOf(0) === None)
    assert(PolygonalNumber(5).indexOf(1).get === 1)
    assert(PolygonalNumber(5).indexOf(5).get === 2)
    assert(PolygonalNumber(5).indexOf(12).get === 3)
    assert(PolygonalNumber(5).indexOf(22).get === 4)
    assert(PolygonalNumber(5).indexOf(35).get === 5)
    assert(PolygonalNumber(5).indexOf(15) === None)
    assert(PolygonalNumber(5).indexOfFractional(15) > PolygonalNumber(5).indexOf(12).get)
    assert(PolygonalNumber(5).indexOfFractional(15) < PolygonalNumber(5).indexOf(22).get)
    assert(PolygonalNumber(5).indexOf(25) === None)
    assert(PolygonalNumber(5).indexOfFractional(25) > PolygonalNumber(5).indexOf(22).get)
    assert(PolygonalNumber(5).indexOfFractional(25) < PolygonalNumber(5).indexOf(35).get)

    assert(PolygonalNumber(6).indexOf(-1) === None)
    assert(PolygonalNumber(6).indexOf(0) === None)
    assert(PolygonalNumber(6).indexOf(1).get === 1)
    assert(PolygonalNumber(6).indexOf(6).get === 2)
    assert(PolygonalNumber(6).indexOf(15).get === 3)
    assert(PolygonalNumber(6).indexOf(28).get === 4)
    assert(PolygonalNumber(6).indexOf(45).get === 5)
    assert(PolygonalNumber(6).indexOf(16) === None)
    assert(PolygonalNumber(6).indexOfFractional(16) > PolygonalNumber(6).indexOf(15).get)
    assert(PolygonalNumber(6).indexOfFractional(16) < PolygonalNumber(6).indexOf(28).get)
    assert(PolygonalNumber(6).indexOf(35) === None)
    assert(PolygonalNumber(6).indexOfFractional(35) > PolygonalNumber(6).indexOf(28).get)
    assert(PolygonalNumber(6).indexOfFractional(35) < PolygonalNumber(6).indexOf(45).get)

    assert(PolygonalNumber(7).indexOf(-1) === None)
    assert(PolygonalNumber(7).indexOf(0) === None)
    assert(PolygonalNumber(7).indexOf(1).get === 1)
    assert(PolygonalNumber(7).indexOf(7).get === 2)
    assert(PolygonalNumber(7).indexOf(18).get === 3)
    assert(PolygonalNumber(7).indexOf(34).get === 4)
    assert(PolygonalNumber(7).indexOf(55).get === 5)
    assert(PolygonalNumber(7).indexOf(45) === None)
    assert(PolygonalNumber(7).indexOfFractional(45) > PolygonalNumber(7).indexOf(34).get)
    assert(PolygonalNumber(7).indexOfFractional(45) < PolygonalNumber(7).indexOf(55).get)
    assert(PolygonalNumber(7).indexOf(50) === None)
    assert(PolygonalNumber(7).indexOfFractional(50) > PolygonalNumber(7).indexOf(34).get)
    assert(PolygonalNumber(7).indexOfFractional(50) < PolygonalNumber(7).indexOf(55).get)

    assert(PolygonalNumber(8).indexOf(-1) === None)
    assert(PolygonalNumber(8).indexOf(0) === None)
    assert(PolygonalNumber(8).indexOf(1).get === 1)
    assert(PolygonalNumber(8).indexOf(8).get === 2)
    assert(PolygonalNumber(8).indexOf(21).get === 3)
    assert(PolygonalNumber(8).indexOf(40).get === 4)
    assert(PolygonalNumber(8).indexOf(65).get === 5)
    assert(PolygonalNumber(8).indexOf(50) === None)
    assert(PolygonalNumber(8).indexOfFractional(50) > PolygonalNumber(8).indexOf(40).get)
    assert(PolygonalNumber(8).indexOfFractional(50) < PolygonalNumber(8).indexOf(65).get)
    assert(PolygonalNumber(8).indexOf(60) === None)
    assert(PolygonalNumber(8).indexOfFractional(60) > PolygonalNumber(8).indexOf(40).get)
    assert(PolygonalNumber(8).indexOfFractional(60) < PolygonalNumber(8).indexOf(65).get)
  }

  test("isNumber checks if a number is polygonal") {
    assert(PolygonalNumber(3).isNumber(8256))
    assert(PolygonalNumber(4).isNumber(5625))
    assert(PolygonalNumber(7).isNumber(2512))
    assert(PolygonalNumber(8).isNumber(1281))
    assert(PolygonalNumber(6).isNumber(8128))
    assert(PolygonalNumber(5).isNumber(2882))
  }

  test("numberNext outputs next polygonal number larger than the number specified") {
    assert(PolygonalNumber(3).numberNext(-1) === 1)
    assert(PolygonalNumber(3).numberNext(0) === 1)
    assert(PolygonalNumber(3).numberNext(1) === 3)
    assert(PolygonalNumber(3).numberNext(2) === 3)
    assert(PolygonalNumber(3).numberNext(3) === 6)
    assert(PolygonalNumber(3).numberNext(9) === 10)
    assert(PolygonalNumber(3).numberNext(10) === 15)
    assert(PolygonalNumber(3).numberNext(11) === 15)

    assert(PolygonalNumber(4).numberNext(-1) === 1)
    assert(PolygonalNumber(4).numberNext(0) === 1)
    assert(PolygonalNumber(4).numberNext(1) === 4)
    assert(PolygonalNumber(4).numberNext(2) === 4)
    assert(PolygonalNumber(4).numberNext(4) === 9)
    assert(PolygonalNumber(4).numberNext(15) === 16)
    assert(PolygonalNumber(4).numberNext(16) === 25)
    assert(PolygonalNumber(4).numberNext(17) === 25)

    assert(PolygonalNumber(5).numberNext(-1) === 1)
    assert(PolygonalNumber(5).numberNext(0) === 1)
    assert(PolygonalNumber(5).numberNext(1) === 5)
    assert(PolygonalNumber(5).numberNext(2) === 5)
    assert(PolygonalNumber(5).numberNext(5) === 12)
    assert(PolygonalNumber(5).numberNext(21) === 22)
    assert(PolygonalNumber(5).numberNext(22) === 35)
    assert(PolygonalNumber(5).numberNext(23) === 35)

    assert(PolygonalNumber(6).numberNext(-1) === 1)
    assert(PolygonalNumber(6).numberNext(0) === 1)
    assert(PolygonalNumber(6).numberNext(1) === 6)
    assert(PolygonalNumber(6).numberNext(2) === 6)
    assert(PolygonalNumber(6).numberNext(6) === 15)
    assert(PolygonalNumber(6).numberNext(27) === 28)
    assert(PolygonalNumber(6).numberNext(28) === 45)
    assert(PolygonalNumber(6).numberNext(29) === 45)

    assert(PolygonalNumber(7).numberNext(-1) === 1)
    assert(PolygonalNumber(7).numberNext(0) === 1)
    assert(PolygonalNumber(7).numberNext(1) === 7)
    assert(PolygonalNumber(7).numberNext(2) === 7)
    assert(PolygonalNumber(7).numberNext(7) === 18)
    assert(PolygonalNumber(7).numberNext(33) === 34)
    assert(PolygonalNumber(7).numberNext(34) === 55)
    assert(PolygonalNumber(7).numberNext(35) === 55)

    assert(PolygonalNumber(8).numberNext(-1) === 1)
    assert(PolygonalNumber(8).numberNext(0) === 1)
    assert(PolygonalNumber(8).numberNext(1) === 8)
    assert(PolygonalNumber(8).numberNext(2) === 8)
    assert(PolygonalNumber(8).numberNext(8) === 21)
    assert(PolygonalNumber(8).numberNext(39) === 40)
    assert(PolygonalNumber(8).numberNext(40) === 65)
    assert(PolygonalNumber(8).numberNext(41) === 65)
  }
}
