package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPartition extends FunSuite {
  test("partition() returns number of ways an integer can be expressed as sum of positive integers") {
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

  test("partition() can also make use of precomputed partition() values, even if none provided") {
    assert(Partition.partition(-1, Map()) === 0)
    assert(Partition.partition(0, Map()) === 1)
    assert(Partition.partition(1, Map()) === 1)
    assert(Partition.partition(2, Map()) === 2)
    assert(Partition.partition(3, Map()) === 3)
    assert(Partition.partition(4, Map()) === 5)
    assert(Partition.partition(5, Map()) === 7)
    assert(Partition.partition(6, Map()) === 11)
    assert(Partition.partition(7, Map()) === 15)
    assert(Partition.partition(8, Map()) === 22)
    assert(Partition.partition(9, Map()) === 30)
    assert(Partition.partition(10, Map()) === 42)
    assert(Partition.partition(11, Map()) === 56)
    assert(Partition.partition(12, Map()) === 77)
    assert(Partition.partition(13, Map()) === 101)
    assert(Partition.partition(14, Map()) === 135)
    assert(Partition.partition(15, Map()) === 176)
    assert(Partition.partition(16, Map()) === 231)
  }

  test("partition() can also make use of precomputed partition() values") {
    assert(Partition.partition(-1, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 0)
    assert(Partition.partition(0, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 1)
    assert(Partition.partition(1, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 1)
    assert(Partition.partition(2, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 2)
    assert(Partition.partition(3, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 3)
    assert(Partition.partition(4, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 5)
    assert(Partition.partition(5, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 7)
    assert(Partition.partition(6, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 11)
    assert(Partition.partition(7, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 15)
    assert(Partition.partition(8, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 22)
    assert(Partition.partition(9, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 30)
    assert(Partition.partition(10, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 42)
    assert(Partition.partition(11, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 56)
    assert(Partition.partition(12, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 77)
    assert(Partition.partition(13, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 101)
    assert(Partition.partition(14, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 135)
    assert(Partition.partition(15, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 176)
    assert(Partition.partition(16, Map(0L -> 1, 1L -> 1, 2L -> 2, 3L -> 3, 4L -> 5, 5L -> 7, 6L -> 11, 7L -> 15, 8L -> 22)) === 231)
  }

  test("partitions() returns a map of all partition() values from 0 to specified integer") {
    assert(Partition.partitions(16) === Map(0 -> 1, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 5, 5 -> 7, 6 -> 11, 7 -> 15, 8 -> 22,
      9 -> 30, 10 -> 42, 11 -> 56, 12 -> 77, 13 -> 101, 14 -> 135, 15 -> 176, 16 -> 231))
  }
}
