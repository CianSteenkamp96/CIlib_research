package cilib.research.mgpso

import cilib.research.core._
import cilib.{Dist, RVar}
import scalaz.Scalaz._
import scalaz._

case class MGParticle(id: Int,
                      pb: Position,
                      pos: Position,
                      velocity: Position,
                      swarmID: Int,
                      lambda: LambdaStrategy) {
  def updatePB: MGParticle =
    this.copy(pb = this.pos)

  def updatePos(x: Position): MGParticle =
    this.copy(pos = x)

  def updateVelocity(x: Position): MGParticle =
    this.copy(velocity = x)

  def updateLambda: MGParticle =
    this.copy(lambda = this.lambda.update)

  def updateFitness(x: NonEmptyList[Double]): MGParticle =
    this.copy(pos = pos.updateFitness(x))
}

object MGParticle {

  def createCollection(benchmark: Benchmark,
                       lambdaStrategy: LambdaStrategy): RVar[NonEmptyList[MGParticle]] =
    benchmark.controlParameters.swarmSizes.toList.zipWithIndex
      .flatMap(x =>
        if (x._1 >= 1) (1 to x._1).toList.map(_ => x._2)
        else List())
      .zipWithIndex
      .toNel
      .get
      .traverse(x =>
        Position
          .createPosition(benchmark)
          .flatMap(p => {
            Dist.stdUniform.map(lambda =>
              MGParticle(x._2, p, p, p.zeroed, x._1, lambdaStrategy.setValue(lambda)))
          }))

}
