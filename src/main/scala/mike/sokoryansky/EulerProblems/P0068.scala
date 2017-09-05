package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.NGon

/*

See https://projecteuler.net/problem=68 for illustrations

Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.

Working clockwise, and starting from the group of three with the numerically lowest external node
(4,3,2 in this example), each solution can be described uniquely.
For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.

It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
Total	Solution Set
9	4,2,3; 5,3,1; 6,1,2
9	4,3,2; 6,2,1; 5,1,3
10	2,3,5; 4,5,1; 6,1,3
10	2,5,3; 6,3,1; 4,1,5
11	1,4,6; 3,6,2; 5,2,4
11	1,6,4; 5,4,2; 3,2,6
12	1,5,6; 2,6,4; 3,4,5
12	1,6,5; 3,5,4; 2,4,6

By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.

Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings.
What is the maximum 16-digit string for a "magic" 5-gon ring?
 */

class P0068 extends EulerProblem {
  /**
    *  By statement of this problem we know that 10 must be in the outer vertices (because if it's in the inner,
    *  then we would get 17-digit strings instead of 16). We also know that we are looking the for the largest
    *  string value of NGon which means largest possible starting digit in the 16-digit string. Since the strings
    *  are formed using smallest outer value as first, the largest outer leading digit can be 6 with outer vertices
    *  being 6, 7, 8, 9, and 10. Therefore inner vertices are 1, 2, 3, 4, and 5. We form all such permutations without
    *  any more optimization since numbers are low enough already and look for max.
    */
  def run: String = NGon.ngons((6 to 10).toSet, (1 to 5).toSet).filter(_.lineSumsEqual).max.value
}

object P0068 extends App {
  (new P0068).printAnswer()
}