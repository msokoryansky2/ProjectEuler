package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.PolygonalNumber

class P0061 extends EulerProblem {
  def run: String = {
    // Map from 3/4/5/6/7/8 to all polygonal numbers of that polygonalness
    val ps = (3 to 8).map(ness => (ness, PolygonalNumber(ness).numbersOfLength(4))).toMap
    // Map 3..8 to map from all possible two-digit prefixes to polygonal numbers of that polygonalness with that prefix
    val psStart = (3 to 8).map(ness =>
                    (ness, (10 to 99).map(start =>
                      (start, ps(ness).filter(p =>
                        p / 100 == start))).filterNot(_._2.isEmpty).toMap)).toMap
    // Map 3..8 to map from all possible two-digit suffixes to polygonal numbers of that polygonalness with that suffix
    val psEnd = (3 to 8).map(ness =>
                    (ness, (10 to 99).map(end =>
                      (end, ps(ness).filter(p =>
                        p % 100 == end))).filterNot(_._2.isEmpty).toMap)).toMap

    print(psStart)
    print(psEnd)
    "Bye"
  }
}

object P0061 extends App {
  (new P0061).printAnswer()
}