package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestNGon extends FunSuite {
  test("lines returns lines that can be drawn in an NGon, starting with smallest outer") {
    assert(NGon(List(2, 3, 1, 5, 6, 4)).lines === List(List(1, 4, 5), List(2, 5, 6), List(3, 6 ,4)))
  }

  test("lineValues returns values of lines that can be drawn in an NGon, starting with smallest outer") {
    assert(NGon(List(2, 3, 1, 5, 6, 4)).lineValues === List("145", "256", "364"))
  }

  test("value returns values of the NGon") {
    assert(NGon(List(2, 3, 1, 5, 6, 4)).value === "145256364")
  }

  test("NGons that can be rotated into one another are considered equal") {
    assert(NGon(List(1, 2, 3, 4, 5, 6)) === NGon(List(1, 2, 3, 4, 5, 6)))
    assert(NGon(List(1, 2, 3, 4, 5, 6)) === NGon(List(3, 1, 2, 6, 4, 5)))
    assert(NGon(List(1, 2, 3, 4, 5, 6)) === NGon(List(2, 3, 1, 5, 6, 4)))
    assert(NGon(List(1, 2, 3, 4, 5, 6)) != NGon(List(2, 3, 4, 5, 6, 1)))
    assert(NGon(List(1, 2, 3, 4, 5, 6)) != NGon(List(3, 2, 1, 6, 5, 4)))
    assert(NGon(List(1, 2, 3, 4, 5, 6)) != NGon(List(1, 2, 3, 5, 4, 6)))
  }

  test("NGon.ngons returns all possible permutations of NGons for given vertices values") {
    assert(NGon.ngons((1 to 6).toSet).filter(_.lineSumsEqual).map(_.value).max === "432621513")
    assert(NGon.ngons((4 to 6).toSet, (1 to 3).toSet).filter(_.lineSumsEqual).map(_.value).max === "432621513")
  }
}
