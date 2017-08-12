package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestFibonacci extends FunSuite {
  test("fibs returns stream of Fibonacci numbers") {
    val fibs = Fibonacci.fibs(0, 1)
    assert(fibs.head === 0)
    assert(fibs.tail.head === 1)
    assert(fibs.tail.tail.head === 1)
    assert(fibs.tail.tail.tail.head === 2)
    assert(fibs.tail.tail.tail.tail.head === 3)
    assert(fibs.tail.tail.tail.tail.tail.head === 5)
    assert(fibs.tail.tail.tail.tail.tail.tail.head === 8)
    assert(fibs.tail.tail.tail.tail.tail.tail.tail.head === 13)
  }

  test("fibsWithIndex is a stream of tuples of Fibonacci HugePositiveInts with their element number") {
    def fibsWithIndex = Fibonacci.fibsWithIndex(new HugePositiveInt("1"), new HugePositiveInt("1"), 1)
    val el1 = fibsWithIndex.head
    assert(el1._1.value === "1")
    assert(el1._2 === 1)
    val el2 = fibsWithIndex.tail.head
    assert(el2._1.value === "1")
    assert(el2._2 === 2)
    val el3 = fibsWithIndex.tail.tail.head
    assert(el3._1.value === "2")
    assert(el3._2 === 3)
    val el4 = fibsWithIndex.tail.tail.tail.head
    assert(el4._1.value === "3")
    assert(el4._2 === 4)
    val el5 = fibsWithIndex.tail.tail.tail.tail.head
    assert(el5._1.value === "5")
    assert(el5._2 === 5)
  }

  test("fibsSum returns sum of stream of fibs") {
    assert(Fibonacci.fibsSum(0, 1, 10, _ => true) === 20)
    assert(Fibonacci.fibsSum(0, 1, 10, _ % 2 == 0) === 10)
    assert(Fibonacci.fibsSum(0, 1, 15, _ % 2 == 1) === 23)
  }
}
