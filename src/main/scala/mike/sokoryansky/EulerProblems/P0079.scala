package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.String

import scala.io.Source

/*

A common security method used for online banking is to ask the user for three random characters from a passcode.
For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters;
the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, analyse the file so as to determine the shortest
possible secret passcode of unknown length.

  */

class P0079 extends EulerProblem {
  def run: String = String.shortestWithScatterSequences(Source.fromResource("p079_keylog.txt").getLines.toList)
}

object P0079 extends App {
  (new P0079).printAnswer()
}