package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestNumberPyramid extends FunSuite{
  val hat = new SortingHat[Long]((a, b) => a + b, 0, (a, b) => Math.max(a, b), 0)
  val pyramid = NumberPyramid(
    """
      |75
      |95 64
      |17 47 82
      |18 35 87 10
      |20 04 82 47 65
      |19 01 23 75 03 34
      |88 02 77 73 07 63 67
      |99 65 04 28 06 16 70 92
      |41 41 26 56 83 40 80 70 33
      |41 48 72 33 47 32 37 16 94 29
      |53 71 44 65 25 43 91 52 97 51 14
      |70 11 33 28 77 73 17 78 39 68 17 57
      |91 71 52 38 17 14 91 43 58 50 27 29 48
      |63 66 04 68 89 53 67 30 73 16 69 87 40 31
      |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
    """.stripMargin)

  val pyramid2 = NumberPyramid(
    """
      |75
      |95 64
      |17 47 82
      |18 35 87 10
    """.stripMargin)

  test("element returns specific element in a pyramid") {
    assert(pyramid.height === 15)
    assert(pyramid.element(1, 1) === 75)
    assert(pyramid.element(1, 4) === 18)
    assert(pyramid.element(3, 4) === 87)
    assert(pyramid.element(4, 4) === 10)
    assert(pyramid.element(1, 15) === 4)
    assert(pyramid.element(10, 15) === 93)
    assert(pyramid.element(15, 15) === 23)
    intercept[Exception] {
      pyramid.element(0, 1)
    }
    intercept[Exception] {
      pyramid.element(1, 0)
    }
    intercept[Exception] {
      pyramid.element(16, 1)
    }
    intercept[Exception] {
      pyramid.element(1, 16)
    }
    intercept[Exception] {
      pyramid.element(5, 4)
    }
  }

  test("bestPathFromPeak returns best list of nodes to be traversed with a bestness function ") {
    assert(pyramid2.bestPathFromPeak(hat) === List((1, 1), (2, 2), (3, 3), (3, 4)))
  }
}
