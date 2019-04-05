package cilib.research.mgpso

import cilib.{Dist, RNG}
import scalaz.Scalaz._

object RList {
  var list = List[Double]()

  def reset(rng: RNG, runs: Int): Unit =
    list =
      Dist.stdUniform.replicateM(2000 * runs + runs).map(_.flatMap(x => List.fill(50)(x))).eval(rng)

  def drop: Unit =
    list = list.tail

  def getHeadAsList(size: Int) =
    List.fill(size)(list.head)
}
