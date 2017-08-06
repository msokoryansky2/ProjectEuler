package com.msokoryansky.EulerProblems

import scala.io.Source

/*
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names,
begin by sorting it into alphabetical order. Then working out the alphabetical value for each name,
multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,
is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
 */

class P0022 extends EulerProblem {
  def run: String = {
    Source.fromResource("p022_names.txt").getLines.mkString("").toUpperCase.
      replaceAll("[^A-Z,]", "").split(",").sortWith(_ < _).zipWithIndex.
      map(ni => ni._1.toList.map(_.toInt - 'A'.toInt + 1).sum * (ni._2 + 1)).sum.toString
  }
}

object P0022 extends App {
  (new P0022).printAnswer()
}

