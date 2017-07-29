package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP0013 extends FunSuite {
  val p = new P0013

  test("BiggieInt allows + operation on long integers represented as strings") {
    assert(new BiggieInt("abc1234dsfhdh335").biggieInt === "1234335")
    assert(new BiggieInt("asjewe").biggieInt === "0")
    assert(new BiggieInt("").biggieInt === "0")
    assert(new BiggieInt("007").biggieInt === "7")

    assert(new BiggieInt("12345").numberOfDigits === 5)
    assert(new BiggieInt("12345").digit(0) === None)
    assert(new BiggieInt("12345").digit(1) === Some(5))
    assert(new BiggieInt("12345").digit(2) === Some(4))
    assert(new BiggieInt("12345").digit(3) === Some(3))
    assert(new BiggieInt("12345").digit(4) === Some(2))
    assert(new BiggieInt("12345").digit(5) === Some(1))
    assert(new BiggieInt("12345").digit(6) === None)

    assert((new BiggieInt("0") + new BiggieInt("0")).biggieInt === "0")
    assert((new BiggieInt("0") + new BiggieInt("7")).biggieInt === "7")
    assert((new BiggieInt("7") + new BiggieInt("0")).biggieInt === "7")
    assert((new BiggieInt("2") + new BiggieInt("5")).biggieInt === "7")
    assert((new BiggieInt("7") + new BiggieInt("7")).biggieInt === "14")
    assert((new BiggieInt("abc") + new BiggieInt("987654321")).biggieInt === "987654321")
    assert((new BiggieInt("23") + new BiggieInt("53")).biggieInt === "76")
    assert((new BiggieInt("89") + new BiggieInt("98")).biggieInt === "187")
    assert((new BiggieInt("999999") + new BiggieInt("00001")).biggieInt === "1000000")
    assert((new BiggieInt("999999") + new BiggieInt("999999")).biggieInt === "1999998")
  }
}