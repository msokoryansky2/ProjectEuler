package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Calendar, Month, Weekday}

/*
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*/

class P0019 extends EulerProblem {
  def run: String = Calendar.matchingDays((y, m, d) => d == 1 && Calendar.weekday(y, m, d) == Weekday.Sun,
    1901, Month.Jan, 1, 2000, Month.Dec, 31).length.toString
}

object P0019 extends App {
  (new P0019).printAnswer()
}
