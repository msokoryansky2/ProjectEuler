package com.msokoryansky.MathUtils

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
  }

}
