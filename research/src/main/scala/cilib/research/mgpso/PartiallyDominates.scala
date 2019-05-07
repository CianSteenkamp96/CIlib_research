package cilib.research.mgpso
import cilib.research.core.Benchmark
import scalaz._
import Scalaz._

object PartiallyDominates {
	def apply(envX: Benchmark)(particleA: MGParticle, particleB: MGParticle, random_indices: (Int, Int, Int)): Boolean =
		if (particleA.pos.fitness == particleB.pos.fitness){
			false
		} else {
			var check = false
			if (envX.controlParameters.swarmSizes.size == 3){

        val partialParticleA = List[Double](particleA.pos.fitness.toList(0), particleA.pos.fitness.toList(1), particleA.pos.fitness.toList(2))
        val partialParticleB = List[Double](particleA.pos.fitness.toList(0), particleA.pos.fitness.toList(1), particleA.pos.fitness.toList(2))

        partialParticleA.zip(partialParticleB).foldLeft(0)(
					(a, c) => a +
						{
							if (envX.compareDoubles(c._1, c._2)) {
								check = true
								0
							} else if (c._1 == c._2)
								0
							else
								1
						}
				) == 0 && check

			} else {

				val partialParticleA = List[Double](particleA.pos.fitness.toList(random_indices._1), particleA.pos.fitness.toList(random_indices._2), particleA.pos.fitness.toList(random_indices._3))
				val partialParticleB = List[Double](particleA.pos.fitness.toList(random_indices._1), particleA.pos.fitness.toList(random_indices._2), particleA.pos.fitness.toList(random_indices._3))

				partialParticleA.zip(partialParticleB).foldLeft(0)(
					(a, c) => a +
						{
							if (envX.compareDoubles(c._1, c._2)) {
								check = true
								0
							} else if (c._1 == c._2)
								0
							else
								1
						}
				) == 0 && check
			}
		}
}