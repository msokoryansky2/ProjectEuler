import com.msokoryansky.EulerProblems.EulerProblem

object Euler {
  def main(args: Array[String]): Unit = {
    (if (args.length > 0) args.map{_.toInt} else (1 to EulerProblem.NUMBER_PROBLEMS).toArray).foreach {
      EulerProblem(_) match {
        case Some(problem) => problem.printAnswer()
        case None =>
      }
    }
  }
}

