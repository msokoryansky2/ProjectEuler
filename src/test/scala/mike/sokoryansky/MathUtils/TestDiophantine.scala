package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestDiophantine extends FunSuite {
  test("Diophantine.x and Diophantine.y solve for X and Y given the other") {

    /*
    3^^2 – 2×2^^2 = 1
    2^^2 – 3×1^^2 = 1
    9^^2 – 5×4^^2 = 1
    5^^2 – 6×2^^2 = 1
    8^^2 – 7×3^^2 = 1
    */

    val dio2 = Diophantine(1, 2, -2, 2, 1)
    assert(dio2.yFind.get === 2)
    assert(dio2.x(dio2.yFind.get).get === 3)

    val dio3 = Diophantine(1, 2, -3, 2, 1)
    assert(dio3.yFind.get === 1)
    assert(dio3.x(dio3.yFind.get).get === 2)

    val dio5 = Diophantine(1, 2, -5, 2, 1)
    assert(dio5.yFind.get === 4)
    assert(dio5.x(dio5.yFind.get).get === 9)

    val dio6 = Diophantine(1, 2, -6, 2, 1)
    assert(dio6.yFind.get === 2)
    assert(dio6.x(dio6.yFind.get).get === 5)

    val dio7 = Diophantine(1, 2, -7, 2, 1)
    assert(dio7.yFind.get === 3)
    assert(dio7.x(dio7.yFind.get).get === 8)
  }
}
