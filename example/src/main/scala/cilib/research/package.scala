package cilib
package research

import scalaz._
import Scalaz._
import spire.math.Interval
import spire.implicits._

package object research {

  type MGArchive = Archive[MGParticle]

  val placeholderENV = Environment(
    cmp = Comparison.dominance(Min),
    eval = Eval.unconstrained(cilib.benchmarks.Benchmarks.spherical[NonEmptyList, Double])
  )

}
