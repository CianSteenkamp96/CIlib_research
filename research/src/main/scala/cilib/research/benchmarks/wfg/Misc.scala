package cilib.research.benchmarks.wfg

import scalaz.Scalaz._
import scalaz._

object Misc {

  def correct_to_01(a: Double, epsilon: Double = 1.0e-10) = {
    Assert(epsilon >= 0.0)

    val min = 0.0
    val max = 1.0
    val min_epsilon = min - epsilon
    val max_epsilon = max + epsilon

    if (a <= min && a >= min_epsilon) {
      min
    } else if (a >= max && a <= max_epsilon) {
      max
    } else {
      a
    }
  }

  def vector_in_01(l: NonEmptyList[Double]) =
    l.foldLeft(0)((a, c) => a + (if (c < 0.0 || c > 1.0) 1 else 0)) == 0

}
