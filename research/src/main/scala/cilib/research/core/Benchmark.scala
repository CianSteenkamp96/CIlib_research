package cilib.research.core

import cilib._
import cilib.exec._
import cilib.research._
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import spire.math._

case class Benchmark(name: String,
                     f: NonEmptyList[Double] => NonEmptyList[Double],
                     bounds: NonEmptyList[Interval[Double]],
                     controlParameters: ControlParameters) {

  val opt = Min

  def compareDoubles(x: Double, y: Double): Boolean = x < y

  def compare(x: NonEmptyList[Double], y: NonEmptyList[Double]): Boolean = {
    val zipped = x.zip(y)
    var xWins = 0
    var yWins = 0
    zipped.map(t => if (t._1 <= t._2) xWins = xWins + 1 else yWins = yWins + 1)
    xWins >= yWins
  }

  def compareAtIndex(x: NonEmptyList[Double], y: NonEmptyList[Double], index: Int): Boolean =
    x.toList(index) < y.toList(index)

  def toStaticProblem: Process[Task, Problem[Double]] =
    Runner.staticProblem(name + " " + controlParameters.swarmSizes.size + "-objectives", placeholderENV.eval)

}
