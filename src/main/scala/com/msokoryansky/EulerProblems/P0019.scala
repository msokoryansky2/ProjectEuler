package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Calendar, Month, Weekday}

class P0019 extends EulerProblem {
  def run: String = Calendar.matchingDays((y, m, d) => d == 1 && Calendar.weekday(y, m, d) == Weekday.Sun,
    1900, Month.Jan, 1, 1920, Month.Dec, 31).length.toString
}

object P0019 extends App {
  (new P0019).printAnswer()
}
