package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestHugePositiveInt extends FunSuite {
  test("HugeInt allows + operation on long integers represented as strings") {
    assert(new HugePositiveInt("abc1234dsfhdh335").value === "1234335")
    assert(new HugePositiveInt("asjewe").value === "0")
    assert(new HugePositiveInt("").value === "0")
    assert(new HugePositiveInt("007").value === "7")

    assert(new HugePositiveInt("12345").numberOfDigits === 5)
    assert(new HugePositiveInt("12345").digit(0) === None)
    assert(new HugePositiveInt("12345").digit(1) === Some(5))
    assert(new HugePositiveInt("12345").digit(2) === Some(4))
    assert(new HugePositiveInt("12345").digit(3) === Some(3))
    assert(new HugePositiveInt("12345").digit(4) === Some(2))
    assert(new HugePositiveInt("12345").digit(5) === Some(1))
    assert(new HugePositiveInt("12345").digit(6) === None)

    assert((new HugePositiveInt("0") + new HugePositiveInt("0")).value === "0")
    assert((new HugePositiveInt("0") + new HugePositiveInt("7")).value === "7")
    assert((new HugePositiveInt("7") + new HugePositiveInt("0")).value === "7")
    assert((new HugePositiveInt("2") + new HugePositiveInt("5")).value === "7")
    assert((new HugePositiveInt("7") + new HugePositiveInt("7")).value === "14")
    assert((new HugePositiveInt("abc") + new HugePositiveInt("987654321")).value === "987654321")
    assert((new HugePositiveInt("23") + new HugePositiveInt("53")).value === "76")
    assert((new HugePositiveInt("89") + new HugePositiveInt("98")).value === "187")
    assert((new HugePositiveInt("999999") + new HugePositiveInt("00001")).value === "1000000")
    assert((new HugePositiveInt("999999") + new HugePositiveInt("999999")).value === "1999998")
  }

  test("HugeInt allows * operation on long integers represented as strings") {
    assert((new HugePositiveInt("0") * new HugePositiveInt("0")).value === "0")
    assert((new HugePositiveInt("0") * new HugePositiveInt("7")).value === "0")
    assert((new HugePositiveInt("7") * new HugePositiveInt("0")).value === "0")
    assert((new HugePositiveInt("2") * new HugePositiveInt("5")).value === "10")
    assert((new HugePositiveInt("7") * new HugePositiveInt("7")).value === "49")
    assert((new HugePositiveInt("12") * new HugePositiveInt("15")).value === "180")
    assert((new HugePositiveInt("12") * new HugePositiveInt("150")).value === "1800")
    assert((new HugePositiveInt("120") * new HugePositiveInt("15")).value === "1800")
    assert((new HugePositiveInt("120") * new HugePositiveInt("150")).value === "18000")
    assert((new HugePositiveInt("1200") * new HugePositiveInt("1500")).value === "1800000")
  }

  test("compare allows 1/0/-1 comaprison of two HugePositiveInt objects") {
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

  test("factorial allows factorial operation on HugePositiveInt") {
    assert(new HugePositiveInt("0").factorial.value === "1")
    assert(new HugePositiveInt("1").factorial.value === "1")
    assert(new HugePositiveInt("2").factorial.value === "2")
    assert(new HugePositiveInt("3").factorial.value === "6")
    assert(new HugePositiveInt("4").factorial.value === "24")
    assert(new HugePositiveInt("5").factorial.value === "120")
    assert(new HugePositiveInt("6").factorial.value === "720")
    assert(new HugePositiveInt("10").factorial.value === "3628800")
  }

  test("^ allows power operation") {
    assert((HugePositiveInt(3) ^ HugePositiveInt(2)).value === "9")
    assert((HugePositiveInt(5) ^ HugePositiveInt(4)).value === "625")
    assert((HugePositiveInt(10) ^ HugePositiveInt(6)).value === "1000000")
  }
}
