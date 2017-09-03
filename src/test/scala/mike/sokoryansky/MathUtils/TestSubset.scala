package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestSubset extends FunSuite {
  test("subsets can return all subsets of specified size or all subsets, period, if size is not specified") {
    assert(Subset.subsets(Set(1, 2, 3, 4), 0) === Set(Set()))
    assert(Subset.subsets(Set(1, 2, 3, 4), 1) === Set(Set(1), Set(2), Set(3), Set(4)))
    assert(Subset.subsets(Set(1, 2, 3, 4), 2) === Set(Set(1, 2), Set(1, 3), Set(1, 4), Set(2, 3), Set(2, 4), Set(3, 4)))
    assert(Subset.subsets(Set(1, 2, 3, 4), 3) === Set(Set(1, 2, 3), Set(1, 2, 4), Set(1, 3, 4), Set(2, 3, 4)))
    assert(Subset.subsets(Set(1, 2, 3, 4), 4) === Set(Set(1, 2, 3, 4)))

    assert(Subset.subsets(Set(1, 2, 3, 4)) === Subset.subsets(Set(1, 2, 3, 4), 0) ++
                                        Subset.subsets(Set(1, 2, 3, 4), 1) ++
                                        Subset.subsets(Set(1, 2, 3, 4), 2) ++
                                        Subset.subsets(Set(1, 2, 3, 4), 3) ++
                                        Subset.subsets(Set(1, 2, 3, 4), 4))
  }

  test("waysToSelectRFromN returns number of ways to select specified number of elements from a set") {
    intercept[Exception] {
      Subset.waysToSelectRFromN(0, 1)
    }
    intercept[Exception] {
      Subset.waysToSelectRFromN(1, 0)
    }
    intercept[Exception] {
      Subset.waysToSelectRFromN(1, 2)
    }
    assert(Subset.waysToSelectRFromN(5, 3) === 10)
    assert(Subset.waysToSelectRFromN(23, 10) === 1144066)
  }
}
