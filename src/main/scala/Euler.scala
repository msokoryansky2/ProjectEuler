import com.msokoryansky.EulerProblems.EulerProblem

object Euler {
  def main(args: Array[String]) = {
    (if (args.length > 0) args.map{_.toInt} else (1 to EulerProblem.NUMBER_PROBLEMS).toArray).foreach {
      EulerProblem.get(_) match {
        case Some(problem) => println("%4s".format(problem.number) + ": " + problem.answer)
        case None =>
      }
    }
  }
}
