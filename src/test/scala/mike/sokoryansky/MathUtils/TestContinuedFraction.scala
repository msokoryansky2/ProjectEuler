package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestContinuedFraction extends FunSuite {
  test("CF.sqrt returns continued fraction calculation of a square root") {
    assert(CF.sqrt(1).toString === "1")
    assert(CF.sqrt(2).toString === "1;[2]...")
    assert(CF.sqrt(3).toString === "1;[1,2]...")
    assert(CF.sqrt(4).toString === "2")
    assert(CF.sqrt(5).toString === "2;[4]...")
    assert(CF.sqrt(6).toString === "2;[2,4]...")
    assert(CF.sqrt(7).toString === "2;[1,1,1,4]...")
    assert(CF.sqrt(8).toString === "2;[1,4]...")
    assert(CF.sqrt(9).toString === "3")
    assert(CF.sqrt(10).toString === "3;[6]...")
    assert(CF.sqrt(11).toString === "3;[3,6]...")
    assert(CF.sqrt(12).toString === "3;[2,6]...")
    assert(CF.sqrt(13).toString === "3;[1,1,1,1,6]...")
    assert(CF.sqrt(14).toString === "3;[1,2,1,6]...")
    assert(CF.sqrt(15).toString === "3;[1,6]...")
    assert(CF.sqrt(16).toString === "4")
    assert(CF.sqrt(17).toString === "4;[8]...")
    assert(CF.sqrt(18).toString === "4;[4,8]...")
    assert(CF.sqrt(19).toString === "4;[2,1,3,1,2,8]...")
    assert(CF.sqrt(20).toString === "4;[2,8]...")
    assert(CF.sqrt(21).toString === "4;[1,1,2,1,1,8]...")
    assert(CF.sqrt(22).toString === "4;[1,2,4,2,1,8]...")
    assert(CF.sqrt(23).toString === "4;[1,3,1,8]...")

    assert(CF.sqrt(61).toString === "7;[1,4,3,1,2,2,1,3,4,1,14]...")
    assert(CF.sqrt(62).toString === "7;[1,6,1,14]...")
    assert(CF.sqrt(63).toString === "7;[1,14]...")
    assert(CF.sqrt(64).toString === "8")
    assert(CF.sqrt(65).toString === "8;[16]...")
    assert(CF.sqrt(66).toString === "8;[8,16]...")
    assert(CF.sqrt(67).toString === "8;[5,2,1,1,7,1,1,2,5,16]...")
    assert(CF.sqrt(68).toString === "8;[4,16]...")
    assert(CF.sqrt(69).toString === "8;[3,3,1,4,1,3,3,16]...")
    assert(CF.sqrt(70).toString === "8;[2,1,2,1,2,16]...")
    assert(CF.sqrt(71).toString === "8;[2,2,1,7,1,2,2,16]...")
    assert(CF.sqrt(72).toString === "8;[2,16]...")
    assert(CF.sqrt(73).toString === "8;[1,1,5,5,1,1,16]...")
    assert(CF.sqrt(74).toString === "8;[1,1,1,1,16]...")
    assert(CF.sqrt(75).toString === "8;[1,1,1,16]...")
    assert(CF.sqrt(76).toString === "8;[1,2,1,1,5,4,5,1,1,2,1,16]...")

    assert(CF.sqrt(9007).toString === "94;[1,9,1,1,4,2,8,5,1,1,1,2,1,2,6,1,1,1,30,1,62,3,3,5,2,4,1,2,16,1,9,20,1,93,1,20,9,1,16,2,1,4,2,5,3,3,62,1,30,1,1,1,6,2,1,2,1,1,1,5,8,2,4,1,1,9,1,188]...")
  }

  test("CF.element returns CF value at specified index") {
    val sqrt73 = CF.sqrt(73)
    assert(sqrt73.element(0) === 8)
    assert(sqrt73.element(1) === 1)
    assert(sqrt73.element(2) === 1)
    assert(sqrt73.element(3) === 5)
    assert(sqrt73.element(4) === 5)
    assert(sqrt73.element(5) === 1)
    assert(sqrt73.element(6) === 1)
    assert(sqrt73.element(7) === 16)
    assert(sqrt73.element(8) === 1)
    assert(sqrt73.element(9) === 1)
    assert(sqrt73.element(10) === 5)
    assert(sqrt73.element(11) === 5)
    assert(sqrt73.element(12) === 1)
    assert(sqrt73.element(13) === 1)
    assert(sqrt73.element(14) === 16)
    assert(sqrt73.element(15) === 1)
    assert(sqrt73.element(16) === 1)
    assert(sqrt73.element(17) === 5)
    assert(sqrt73.element(18) === 5)
    assert(sqrt73.element(19) === 1)
    assert(sqrt73.element(20) === 1)
    assert(sqrt73.element(21) === 16)

    val cf = CF(123, List(1, 2, 3), List(4, 5, 6, 7))
    assert(cf.element(0) === 123)
    assert(cf.element(1) === 1)
    assert(cf.element(2) === 2)
    assert(cf.element(3) === 3)
    assert(cf.element(4) === 4)
    assert(cf.element(5) === 5)
    assert(cf.element(6) === 6)
    assert(cf.element(7) === 7)
    assert(cf.element(8) === 4)
    assert(cf.element(9) === 5)
    assert(cf.element(10) === 6)
    assert(cf.element(11) === 7)
    assert(cf.element(12) === 4)
    assert(cf.element(13) === 5)
    assert(cf.element(14) === 6)
    assert(cf.element(15) === 7)
  }

  test("CF.toFraction computes specified number of iterations of a continued fraction and returns Fraction object") {
    val sqrt2 = CF.sqrt(2)
    assert(sqrt2.toFraction(0).toString === "1/1")
    assert(sqrt2.toFraction(1).toString === "3/2")
    assert(sqrt2.toFraction(2).toString === "7/5")
    assert(sqrt2.toFraction(3).toString === "17/12")
    assert(sqrt2.toFraction(4).toString === "41/29")
    assert(sqrt2.toFraction(5).toString === "99/70")
    assert(sqrt2.toFraction(6).toString === "239/169")
    assert(sqrt2.toFraction(7).toString === "577/408")
    assert(sqrt2.toFraction(8).toString === "1393/985")
    assert(sqrt2.toFraction(9).toString === "3363/2378")
  }

  test("CF.e approximates e as a continued fraction") {
    val e = CF.e(10)

    assert(e.toString === "2;1,2,1,1,4,1,1,6,1,1")

    assert(e.toFraction(0).toString === "2/1")
    assert(e.toFraction(1).toString === "3/1")
    assert(e.toFraction(2).toString === "8/3")
    assert(e.toFraction(3).toString === "11/4")
    assert(e.toFraction(4).toString === "19/7")
    assert(e.toFraction(5).toString === "87/32")
    assert(e.toFraction(6).toString === "106/39")
    assert(e.toFraction(7).toString === "193/71")
    assert(e.toFraction(8).toString === "1264/465")
    assert(e.toFraction(9).toString === "1457/536")
  }
}
