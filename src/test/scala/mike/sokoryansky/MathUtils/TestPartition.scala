package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPartition extends FunSuite {
  test("Partition.partition returns number of ways an integer can be expressed as sum of positive integers") {
    assert(Partition.partition(-1) === 0)
    assert(Partition.partition(0) === 1)
    assert(Partition.partition(1) === 1)
    assert(Partition.partition(2) === 2)
    assert(Partition.partition(3) === 3)
    assert(Partition.partition(4) === 5)
    assert(Partition.partition(5) === 7)
    assert(Partition.partition(6) === 11)
    assert(Partition.partition(7) === 15)
    assert(Partition.partition(8) === 22)
    assert(Partition.partition(9) === 30)
    assert(Partition.partition(10) === 42)
    assert(Partition.partition(11) === 56)
    assert(Partition.partition(12) === 77)
    assert(Partition.partition(13) === 101)
    assert(Partition.partition(14) === 135)
    assert(Partition.partition(15) === 176)
    assert(Partition.partition(16) === 231)
  }
}
