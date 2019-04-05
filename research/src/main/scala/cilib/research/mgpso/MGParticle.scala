package cilib.research.mgpso

import cilib.RVar
import cilib.research.core._
import scalaz.Scalaz._
import scalaz._

case class MGParticle(id: Int,
                      pb: Position,
                      pos: Position,
                      velocity: Position,
                      swarmID: Int,
                      lambda: LambdaStrategy) {
  def updatePB =
    this.copy(pb = this.pos)

  def updatePos(x: Position) =
    this.copy(pos = x)

  def updateVelocity(x: Position) =
    this.copy(velocity = x)

  def updateLambda =
    this.copy(lambda = this.lambda.update)

  def updateFitness(x: NonEmptyList[Double]) =
    this.copy(pos = pos.updateFitness(x))
}

object MGParticle {

  def createCollection(benchmark: Benchmark, lambdaStrategy: LambdaStrategy): RVar[NonEmptyList[MGParticle]] =
    benchmark
      .controlParameters
      .swarmSizes
      .toList
      .zipWithIndex
      .flatMap(x =>
        if (x._1 >= 1) (1 to x._1).toList.map(_ => x._2)
        else List()
      )
      .toNel
      .get
      .traverse(id =>
        Position
          .createPosition(benchmark)
          .map(p => MGParticle(0, p, p, p.zeroed, id, lambdaStrategy))
      )

}
