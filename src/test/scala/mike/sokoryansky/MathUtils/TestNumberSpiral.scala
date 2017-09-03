package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestNumberSpiral extends FunSuite {
  test("NumberSpiral clockwise returns rows, columns, cells, and diagonals") {
    intercept[Exception] {
      NumberSpiral(0)
    }
    intercept[Exception] {
      NumberSpiral(6)
    }
    val ns = NumberSpiral(5)

    assert(ns.row(0) === Vector(21, 22, 23, 24, 25))
    assert(ns.row(1) === Vector(20, 7, 8, 9, 10))
    assert(ns.row(2) === Vector(19, 6, 1, 2, 11))
    assert(ns.row(3) === Vector(18, 5, 4, 3, 12))
    assert(ns.row(4) === Vector(17, 16, 15, 14, 13))
    intercept[Exception] {
      ns.row(-1)
    }
    intercept[Exception] {
      ns.row(5)
    }

    assert(ns.col(0) === Vector(21, 20, 19, 18, 17))
    assert(ns.col(1) === Vector(22, 7, 6, 5, 16))
    assert(ns.col(2) === Vector(23, 8, 1, 4, 15))
    assert(ns.col(3) === Vector(24, 9, 2, 3, 14))
    assert(ns.col(4) === Vector(25, 10, 11, 12, 13))
    intercept[Exception] {
      ns.col(-1)
    }
    intercept[Exception] {
      ns.col(5)
    }

    assert(ns.cell(2, 4) === 15)
    assert(ns.cell(0, 4) === 17)
    intercept[Exception] {
      ns.cell(0, 5)
    }
    intercept[Exception] {
      ns.cell(5, 0)
    }

    assert(ns.diag1 === Vector(21, 7, 1, 3, 13))
    assert(ns.diag2 === Vector(25, 9, 1, 5, 17))

    val innerSpiral = ns.trim(1)

    assert(innerSpiral.col(0) === Vector(7, 6, 5))
    assert(innerSpiral.col(1) === Vector(8, 1, 4))
    assert(innerSpiral.col(2) === Vector(9, 2, 3))

    assert(innerSpiral.row(0) === Vector(7, 8, 9))
    assert(innerSpiral.row(1) === Vector(6, 1, 2))
    assert(innerSpiral.row(2) === Vector(5, 4, 3))
  }

  test("NumberSpiral counter-clockwise returns rows, columns, cells, and diagonals") {
    intercept[Exception] {
      NumberSpiral(0, clockwise = NumberSpiral.Clockwise.CCW)
    }
    intercept[Exception] {
      NumberSpiral(6, clockwise = NumberSpiral.Clockwise.CCW)
    }
    val ns = NumberSpiral(5, clockwise = NumberSpiral.Clockwise.CCW)

    assert(ns.row(0) === Vector(17, 16, 15, 14, 13))
    assert(ns.row(1) === Vector(18, 5, 4, 3, 12))
    assert(ns.row(2) === Vector(19, 6, 1, 2, 11))
    assert(ns.row(3) === Vector(20, 7, 8, 9, 10))
    assert(ns.row(4) === Vector(21, 22, 23, 24, 25))
    intercept[Exception] {
      ns.row(-1)
    }
    intercept[Exception] {
      ns.row(5)
    }

    assert(ns.col(0) === Vector(17, 18, 19, 20, 21))
    assert(ns.col(1) === Vector(16, 5, 6, 7, 22))
    assert(ns.col(2) === Vector(15, 4, 1, 8, 23))
    assert(ns.col(3) === Vector(14, 3, 2, 9, 24))
    assert(ns.col(4) === Vector(13, 12, 11, 10, 25))
    intercept[Exception] {
      ns.col(-1)
    }
    intercept[Exception] {
      ns.col(5)
    }

    assert(ns.cell(2, 4) === 23)
    assert(ns.cell(0, 4) === 21)
    intercept[Exception] {
      ns.cell(0, 5)
    }
    intercept[Exception] {
      ns.cell(5, 0)
    }

    assert(ns.diag1 === Vector(17, 5, 1, 9, 25))
    assert(ns.diag2 === Vector(13, 3, 1, 7, 21))

    val innerSpiral = ns.trim(1)

    assert(innerSpiral.col(0) === Vector(5, 6, 7))
    assert(innerSpiral.col(1) === Vector(4, 1, 8))
    assert(innerSpiral.col(2) === Vector(3, 2, 9))

    assert(innerSpiral.row(0) === Vector(5, 4, 3))
    assert(innerSpiral.row(1) === Vector(6, 1, 2))
    assert(innerSpiral.row(2) === Vector(7, 8, 9))
  }

  test("find finds smallest spiral matching predicate") {
    val ns = NumberSpiral(9)
    val sp2 = NumberSpiral.find(1, NumberSpiral.Direction.R, NumberSpiral.Clockwise.CW, s => s.diag1.length == 9).get
    assert(sp2.sideLength === 9)
    assert(sp2.diag1 === ns.diag1)
    assert(sp2.diag2 === ns.diag2)
    val sp3 = NumberSpiral.find(1, NumberSpiral.Direction.R, NumberSpiral.Clockwise.CW, s => s.diag1.length == 129).get
    assert(sp3.sideLength === 129)
  }

  test("corners and cornersFind allow findings smallest spiral size whose corners match specified predicate") {
    assert(NumberSpiral.corners().head === (1, Seq(1)))
    assert(NumberSpiral.corners().tail.head === (3, Seq(9, 7, 5, 3)))
    assert(NumberSpiral.corners().tail.tail.head === (5, Seq(25, 21, 17, 13)))
    assert(NumberSpiral.corners().tail.tail.tail.head === (7, Seq(49, 43, 37, 31)))
    assert(NumberSpiral.corners().tail.tail.tail.tail.head === (9, Seq(81, 73, 65, 57)))

    assert(NumberSpiral.cornersFind(_.sum > 30, 1, 10) === Some(Seq(25, 21, 17, 13, 9, 7, 5, 3, 1)))
    assert(NumberSpiral.cornersFind(_.sum < 1, 1, 10) === None)

    assert(NumberSpiral.cornersFind((tally, _) => tally >= 3, (c) => c % 3 == 0, 1, 10) ===
      Some(Seq(25, 21, 17, 13, 9, 7, 5, 3, 1)))
    assert(NumberSpiral.cornersFind((tally, _) => tally >= 3, (c) => c < 10, 1, 10) ===
      Some(Seq(9, 7, 5, 3, 1)))
  }
}
