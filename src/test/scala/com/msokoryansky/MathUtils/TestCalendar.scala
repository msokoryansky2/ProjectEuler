package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestCalendar extends FunSuite {
  test("dayNumber returns number of specified data relative to day #1 which is 1 Jan 1900") {
    intercept[Exception] {
      Calendar.dayNumber(1899, Month.Jan, 1)
    }
    intercept[Exception] {
      Calendar.dayNumber(1900, Month.Jan, 0)
    }
    intercept[Exception] {
      Calendar.dayNumber(1900, Month.Jan, 32)
    }
    intercept[Exception] {
      Calendar.dayNumber(1900, Month.Feb, 29)
    }
    assert(Calendar.dayNumber(1900, Month.Jan, 1) === 1)
    assert(Calendar.dayNumber(1900, Month.Jan, 31) === 31)
    //assert(Calendar.dayNumber(1900, Month.Feb, 28) === 59)
    //assert(Calendar.dayNumber(1900, Month.Mar, 1) === 60)
    assert(Calendar.dayNumber(1901, Month.Jan, 1) === 366)
  }
}
