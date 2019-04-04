package cilib

import cilib.research.core.Archive
import cilib.research.mgpso.MGParticle
import scalaz._
import spire.implicits._

package object research {

  type MGArchive = Archive[MGParticle]

  val placeholderENV = Environment(
    cmp = Comparison.dominance(Min),
    eval = Eval.unconstrained(cilib.benchmarks.Benchmarks.spherical[NonEmptyList, Double])
  )

}
