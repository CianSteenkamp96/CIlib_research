package cilib.research.mgpso

import cilib.research.core._
import cilib.{Dist, RNG}
import scalaz.Scalaz._

object RList {
  var list = List[Double]()

  def reset(rng: RNG, runs: Int): Unit =
    list =
      Dist.stdUniform.replicateM(2000 * runs + runs).map(_.flatMap(x => List.fill(50)(x))).eval(rng)

  def drop: Unit =
    list = list.tail

  def getHeadAsList(envX: EnvironmentX) =
    List.fill(envX.bounds.size)(list.head)
}
