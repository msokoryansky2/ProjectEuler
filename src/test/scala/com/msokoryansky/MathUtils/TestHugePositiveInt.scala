package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestHugePositiveInt extends FunSuite {
  test("HugeInt allows + operation on long integers represented as strings") {
    assert(new HugePositiveInt("abc1234dsfhdh335").hugePositiveInt === "1234335")
    assert(new HugePositiveInt("asjewe").hugePositiveInt === "0")
    assert(new HugePositiveInt("").hugePositiveInt === "0")
    assert(new HugePositiveInt("007").hugePositiveInt === "7")

    assert(new HugePositiveInt("12345").numberOfDigits === 5)
    assert(new HugePositiveInt("12345").digit(0) === None)
    assert(new HugePositiveInt("12345").digit(1) === Some(5))
    assert(new HugePositiveInt("12345").digit(2) === Some(4))
    assert(new HugePositiveInt("12345").digit(3) === Some(3))
    assert(new HugePositiveInt("12345").digit(4) === Some(2))
    assert(new HugePositiveInt("12345").digit(5) === Some(1))
    assert(new HugePositiveInt("12345").digit(6) === None)

    assert((new HugePositiveInt("0") + new HugePositiveInt("0")).hugePositiveInt === "0")
    assert((new HugePositiveInt("0") + new HugePositiveInt("7")).hugePositiveInt === "7")
    assert((new HugePositiveInt("7") + new HugePositiveInt("0")).hugePositiveInt === "7")
    assert((new HugePositiveInt("2") + new HugePositiveInt("5")).hugePositiveInt === "7")
    assert((new HugePositiveInt("7") + new HugePositiveInt("7")).hugePositiveInt === "14")
    assert((new HugePositiveInt("abc") + new HugePositiveInt("987654321")).hugePositiveInt === "987654321")
    assert((new HugePositiveInt("23") + new HugePositiveInt("53")).hugePositiveInt === "76")
    assert((new HugePositiveInt("89") + new HugePositiveInt("98")).hugePositiveInt === "187")
    assert((new HugePositiveInt("999999") + new HugePositiveInt("00001")).hugePositiveInt === "1000000")
    assert((new HugePositiveInt("999999") + new HugePositiveInt("999999")).hugePositiveInt === "1999998")
  }

  test("HugeInt allows * operation on long integers represented as strings") {
    assert((new HugePositiveInt("0") * new HugePositiveInt("0")).hugePositiveInt === "0")
    assert((new HugePositiveInt("0") * new HugePositiveInt("7")).hugePositiveInt === "0")
    assert((new HugePositiveInt("7") * new HugePositiveInt("0")).hugePositiveInt === "0")
    assert((new HugePositiveInt("2") * new HugePositiveInt("5")).hugePositiveInt === "10")
    assert((new HugePositiveInt("7") * new HugePositiveInt("7")).hugePositiveInt === "49")
    assert((new HugePositiveInt("12") * new HugePositiveInt("15")).hugePositiveInt === "180")
    assert((new HugePositiveInt("12") * new HugePositiveInt("150")).hugePositiveInt === "1800")
    assert((new HugePositiveInt("120") * new HugePositiveInt("15")).hugePositiveInt === "1800")
    assert((new HugePositiveInt("120") * new HugePositiveInt("150")).hugePositiveInt === "18000")
    assert((new HugePositiveInt("1200") * new HugePositiveInt("1500")).hugePositiveInt === "1800000")
  }

  test("compare allows 1/0/-1 comaprison of two HugeInt objects") {
    assert(new HugePositiveInt("0") < new HugePositiveInt("1"))
    assert(new HugePositiveInt("10") < new HugePositiveInt("11"))
    assert(new HugePositiveInt("100") < new HugePositiveInt("101"))
    assert(new HugePositiveInt("100") < new HugePositiveInt("1001"))
    assert(new HugePositiveInt("10") <= new HugePositiveInt("10"))
    assert(new HugePositiveInt("10") >= new HugePositiveInt("10"))
    assert(new HugePositiveInt("10") >= new HugePositiveInt("10"))
    assert(new HugePositiveInt("1") > new HugePositiveInt("0"))
    assert(new HugePositiveInt("11") > new HugePositiveInt("10"))
    assert(new HugePositiveInt("101") > new HugePositiveInt("100"))
    assert(new HugePositiveInt("1001") > new HugePositiveInt("100"))
    assert(new HugePositiveInt("766") > new HugePositiveInt("765"))
    assert(new HugePositiveInt("999") < new HugePositiveInt("1000"))
  }

  test("factorial allows factorial operation on HugeInt") {
    assert(new HugePositiveInt("0").factorial.hugePositiveInt === "1")
    assert(new HugePositiveInt("1").factorial.hugePositiveInt === "1")
    assert(new HugePositiveInt("2").factorial.hugePositiveInt === "2")
    assert(new HugePositiveInt("3").factorial.hugePositiveInt === "6")
    assert(new HugePositiveInt("4").factorial.hugePositiveInt === "24")
    assert(new HugePositiveInt("5").factorial.hugePositiveInt === "120")
    assert(new HugePositiveInt("6").factorial.hugePositiveInt === "720")
    assert(new HugePositiveInt("10").factorial.hugePositiveInt === "3628800")
  }
}
