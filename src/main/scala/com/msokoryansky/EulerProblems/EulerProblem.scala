package com.msokoryansky.EulerProblems

import java.util.Scanner

abstract class EulerProblem {
  lazy val answer: String = this.run
  /**
    * @return String value representing answer to the given problem, converted to String
    */
  def run : String
  /**
    * @return Int number of the problem in Project Euler assuming its class name has the number as suffix
    */
  def number : Int =  new Scanner(this.getClass.getName.reverse).useDelimiter("[^0-9]+").nextInt.toString.reverse.toInt

  def main(args: Array[String]) = answer
}

object EulerProblem {
  val NUMBER_PROBLEMS = 608

  /**
    * @param i Int number of Project Euler problem
    * @return Option with either Some(EulerProblem) object or None if requested number isn't implemented
    */
  def get(i: Int) : Option[EulerProblem] = {
    try {
      Some(Class.forName("com.msokoryansky.EulerProblems.P" + i.toString).newInstance.asInstanceOf[EulerProblem])
    } catch {
      case _ : Exception => None
    }
  }
}