package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestHugeInt extends FunSuite {
  test("BiggieInt allows + operation on long integers represented as strings") {
    assert(new HugeInt("abc1234dsfhdh335").hugeInt === "1234335")
    assert(new HugeInt("asjewe").hugeInt === "0")
    assert(new HugeInt("").hugeInt === "0")
    assert(new HugeInt("007").hugeInt === "7")

    assert(new HugeInt("12345").numberOfDigits === 5)
    assert(new HugeInt("12345").digit(0) === None)
    assert(new HugeInt("12345").digit(1) === Some(5))
    assert(new HugeInt("12345").digit(2) === Some(4))
    assert(new HugeInt("12345").digit(3) === Some(3))
    assert(new HugeInt("12345").digit(4) === Some(2))
    assert(new HugeInt("12345").digit(5) === Some(1))
    assert(new HugeInt("12345").digit(6) === None)

    assert((new HugeInt("0") + new HugeInt("0")).hugeInt === "0")
    assert((new HugeInt("0") + new HugeInt("7")).hugeInt === "7")
    assert((new HugeInt("7") + new HugeInt("0")).hugeInt === "7")
    assert((new HugeInt("2") + new HugeInt("5")).hugeInt === "7")
    assert((new HugeInt("7") + new HugeInt("7")).hugeInt === "14")
    assert((new HugeInt("abc") + new HugeInt("987654321")).hugeInt === "987654321")
    assert((new HugeInt("23") + new HugeInt("53")).hugeInt === "76")
    assert((new HugeInt("89") + new HugeInt("98")).hugeInt === "187")
    assert((new HugeInt("999999") + new HugeInt("00001")).hugeInt === "1000000")
    assert((new HugeInt("999999") + new HugeInt("999999")).hugeInt === "1999998")
  }

  test("BiggieInt allows * operation on long integers represented as strings") {
    assert((new HugeInt("0") * new HugeInt("0")).hugeInt === "0")
    assert((new HugeInt("0") * new HugeInt("7")).hugeInt === "0")
    assert((new HugeInt("7") * new HugeInt("0")).hugeInt === "0")
    assert((new HugeInt("2") * new HugeInt("5")).hugeInt === "10")
    assert((new HugeInt("7") * new HugeInt("7")).hugeInt === "49")
    assert((new HugeInt("12") * new HugeInt("15")).hugeInt === "180")
  }
}
