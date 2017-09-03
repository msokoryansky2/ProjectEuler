package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestContinuedFraction extends FunSuite {
  test("CFSqrt.sqrt returns continued fraction calculation of a square root") {
    assert(CFSqrt.sqrt(1).toString === "1")
    assert(CFSqrt.sqrt(2).toString === "1;[2]...")
    assert(CFSqrt.sqrt(3).toString === "1;[1,2]...")
    assert(CFSqrt.sqrt(4).toString === "2")
    assert(CFSqrt.sqrt(5).toString === "2;[4]...")
    assert(CFSqrt.sqrt(6).toString === "2;[2,4]...")
    assert(CFSqrt.sqrt(7).toString === "2;[1,1,1,4]...")
    assert(CFSqrt.sqrt(8).toString === "2;[1,4]...")
    assert(CFSqrt.sqrt(9).toString === "3")
    assert(CFSqrt.sqrt(10).toString === "3;[6]...")
    assert(CFSqrt.sqrt(11).toString === "3;[3,6]...")
    assert(CFSqrt.sqrt(12).toString === "3;[2,6]...")
    assert(CFSqrt.sqrt(13).toString === "3;[1,1,1,1,6]...")
    assert(CFSqrt.sqrt(14).toString === "3;[1,2,1,6]...")
    assert(CFSqrt.sqrt(15).toString === "3;[1,6]...")
    assert(CFSqrt.sqrt(16).toString === "4")
    assert(CFSqrt.sqrt(17).toString === "4;[8]...")
    assert(CFSqrt.sqrt(18).toString === "4;[4,8]...")
    assert(CFSqrt.sqrt(19).toString === "4;[2,1,3,1,2,8]...")
    assert(CFSqrt.sqrt(20).toString === "4;[2,8]...")
    assert(CFSqrt.sqrt(21).toString === "4;[1,1,2,1,1,8]...")
    assert(CFSqrt.sqrt(22).toString === "4;[1,2,4,2,1,8]...")
    assert(CFSqrt.sqrt(23).toString === "4;[1,3,1,8]...")

    assert(CFSqrt.sqrt(61).toString === "7;[1,4,3,1,2,2,1,3,4,1,14]...")
    assert(CFSqrt.sqrt(62).toString === "7;[1,6,1,14]...")
    assert(CFSqrt.sqrt(63).toString === "7;[1,14]...")
    assert(CFSqrt.sqrt(64).toString === "8")
    assert(CFSqrt.sqrt(65).toString === "8;[16]...")
    assert(CFSqrt.sqrt(66).toString === "8;[8,16]...")
    assert(CFSqrt.sqrt(67).toString === "8;[5,2,1,1,7,1,1,2,5,16]...")
    assert(CFSqrt.sqrt(68).toString === "8;[4,16]...")
    assert(CFSqrt.sqrt(69).toString === "8;[3,3,1,4,1,3,3,16]...")
    assert(CFSqrt.sqrt(70).toString === "8;[2,1,2,1,2,16]...")
    assert(CFSqrt.sqrt(71).toString === "8;[2,2,1,7,1,2,2,16]...")
    assert(CFSqrt.sqrt(72).toString === "8;[2,16]...")
    assert(CFSqrt.sqrt(73).toString === "8;[1,1,5,5,1,1,16]...")
    assert(CFSqrt.sqrt(74).toString === "8;[1,1,1,1,16]...")
    assert(CFSqrt.sqrt(75).toString === "8;[1,1,1,16]...")
    assert(CFSqrt.sqrt(76).toString === "8;[1,2,1,1,5,4,5,1,1,2,1,16]...")

    assert(CFSqrt.sqrt(9007).toString === "94;[1,9,1,1,4,2,8,5,1,1,1,2,1,2,6,1,1,1,30,1,62,3,3,5,2,4,1,2,16,1,9,20,1,93,1,20,9,1,16,2,1,4,2,5,3,3,62,1,30,1,1,1,6,2,1,2,1,1,1,5,8,2,4,1,1,9,1,188]...")
  }
}
