package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestLogical extends FunSuite {
  test("xor xors chars and strings given source and key") {
    assert(Logical.xor('A', '*') === 'k')
    assert(Logical.xor('*', 'A') === 'k')
    assert(Logical.xor('k', '*') === 'A')
    assert(Logical.xor('*', 'k') === 'A')
    assert(Logical.xor('k', 'A') === '*')
    assert(Logical.xor('A', 'k') === '*')

    assert(Logical.xor("A", "*") === "k")
    assert(Logical.xor("*", "A") === "k")
    assert(Logical.xor("k", "*") === "A")
    assert(Logical.xor("*", "k") === "A")
    assert(Logical.xor("k", "A") === "*")
    assert(Logical.xor("A", "k") === "*")

    assert(Logical.xor("kAkA", "*") === "AkAk")
    assert(Logical.xor("kAkA", "**") === "AkAk")
    assert(Logical.xor("kAkA", "***") === "AkAk")
    assert(Logical.xor("kAkA", "****") === "AkAk")
    assert(Logical.xor("kAkA", "*****") === "AkAk")

    assert(Logical.xor("AkAk", "*") === "kAkA")
    assert(Logical.xor("AkAk", "**") === "kAkA")
    assert(Logical.xor("AkAk", "***") === "kAkA")
    assert(Logical.xor("AkAk", "****") === "kAkA")
    assert(Logical.xor("AkAk", "*****") === "kAkA")

    assert(Logical.xor("kAA*Ak**Ak*", "Ak*") === "**kk*AkAk*A")
    assert(Logical.xor("**kk*AkAk*A", "Ak*") === "kAA*Ak**Ak*")
  }
}
