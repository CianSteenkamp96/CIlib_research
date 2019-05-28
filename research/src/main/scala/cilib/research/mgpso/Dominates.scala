package cilib.research.mgpso
import cilib.research.core.Benchmark
import scalaz.Scalaz._

object Dominates {
  def apply(envX: Benchmark)(particleA: MGParticle, particleB: MGParticle, notUsed: Option[PartialDominance]): Boolean =
    if (particleA.pos.fitness == particleB.pos.fitness) {
      false
    } else {
      var check = false
      particleA.pos.fitness
        .zip(particleB.pos.fitness)
        .foldLeft(0)((a, c) =>
          a + {
            if (envX.compareDoubles(c._1, c._2)) {
              check = true // flag to check that @ least one strictly better
              0
            } else if (c._1 == c._2) 0
            else 1
        }) == 0 && check
    }
}
