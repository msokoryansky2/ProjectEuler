package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{PolygonalNumber6, PolygonalNumber5, PolygonalNumber3}

/*

Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
Triangle 	  	Tn=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
Pentagonal 	  	Pn=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
Hexagonal 	  	Hn=n(2n−1) 	  	1, 6, 15, 28, 45, ...

It can be verified that T285 = P165 = H143 = 40755.

Find the next triangle number that is also pentagonal and hexagonal.

 */

class P0045 extends EulerProblem {
  def run: String = PolygonalNumber5.pentagonalNumbers()
    .find(n => PolygonalNumber6.isHexagonalNumber(n) && PolygonalNumber3.isTriangleNumber(n) && n > 40755).get.toString
}

object P0045 extends App {
  (new P0045).printAnswer()
}