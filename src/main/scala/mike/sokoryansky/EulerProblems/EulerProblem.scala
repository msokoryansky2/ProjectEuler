package mike.sokoryansky.EulerProblems

import java.util.Scanner

abstract class EulerProblem {
  lazy val answer: String = this.run
  /**
    * @return String value representing answer to the given problem, converted to String
    */
  def run: String
  /**
    * @return Int number of the problem in Project Euler assuming its class name has the number as suffix
    */
  def number: Int =  new Scanner(this.getClass.getName).useDelimiter("[^0-9]+").nextInt

  def printAnswer(): Unit = println("%4s".format(number) + ": " + answer)
}

object EulerProblem {
  val NUMBER_PROBLEMS = 608

  /**
    * Fully-qualified with package class name for any given Euler Project problem number
    * @param i number of Project Euler problem
    * @return fully-qualified class name (may or may not be implemented) for corresponding problem number
    */
  def classEulerProblem(i: Int): String =  "mike.sokoryansky.EulerProblems.P" + "%04d".format(i)

  /**
    * @param i Int number of Project Euler problem
    * @return Option with either Some(EulerProblem) object or None if requested number isn't implemented
    */
  def apply(i: Int): Option[EulerProblem] = {
    try {
      Some(Class.forName(classEulerProblem(i)).newInstance.asInstanceOf[EulerProblem])
    } catch {
      case _ : Exception => None
    }
  }
}