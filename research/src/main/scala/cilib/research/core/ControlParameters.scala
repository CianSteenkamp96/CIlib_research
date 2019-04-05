package cilib.research.core
import scalaz.NonEmptyList

case class ControlParameters(w: Double,
                             c1: Double,
                             c2: Double,
                             c3: Double,
                             swarmSizes: NonEmptyList[Int])
