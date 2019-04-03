package cilib
package research

import scalaz._
import Scalaz._
import cilib.exec.{Problem, Runner}
import cilib.research.research.placeholderENV
import scalaz.concurrent.Task
import scalaz.stream.Process
import spire.math._

case class ControlParameters(w: Double,
                             c1: Double,
                             c2: Double,
                             c3: Double,
                             swarmSizes: NonEmptyList[Int])

case class EnvironmentX(name: String,
                        f: NonEmptyList[Double] => NonEmptyList[Double],
                        bounds: NonEmptyList[Interval[Double]],
                        cp: ControlParameters) {

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
    Runner.staticProblem(name + " " + cp.swarmSizes.size + "-objectives", placeholderENV.eval)

}
