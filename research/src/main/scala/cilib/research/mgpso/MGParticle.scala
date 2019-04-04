package cilib.research.mgpso

import cilib.research.core._
import cilib.{Dist, RNG, RVar}
import scalaz.Scalaz._
import scalaz._


case class Lambda(value: RVar[NonEmptyList[Double]],
                  next: RVar[NonEmptyList[Double]] => RVar[NonEmptyList[Double]]) {
  def update: Lambda =
    Lambda(next(value), next)
}

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

object Lambda {

  def linearIncreasing(x: Double, envX: EnvironmentX): Lambda =
    new Lambda(RVar.pure(List.fill(envX.bounds.size)(0.0).toNel.get),
               rl => rl.map(_.map(x => x + 0.0005)))

  def linearDecreasing(x: Double, envX: EnvironmentX): Lambda =
    new Lambda(RVar.pure(List.fill(envX.bounds.size)(1.0).toNel.get),
               rl => rl.map(_.map(x => x - 0.0005)))

  def std(x: Double, envX: EnvironmentX): Lambda =
    new Lambda(RVar.pure(List.fill(envX.bounds.size)(x).toNel.get), rl => rl)

  def random(x: Double, envX: EnvironmentX): Lambda =
    new Lambda({
      val value = RList.getHeadAsList(envX).toNel.get
      RList.drop
      RVar.pure(value)
    }, _ => {
      val value = RList.getHeadAsList(envX).toNel.get
      RList.drop
      RVar.pure(value)
    })

  def random_i(x: Double, envX: EnvironmentX): Lambda =
    new Lambda(Dist.stdUniform.map(value => List.fill(envX.bounds.size)(value).toNel.get),
               _ => Dist.stdUniform.map(value => List.fill(envX.bounds.size)(value).toNel.get))

  def random_i_j(x: Double, envX: EnvironmentX): Lambda =
    new Lambda(Dist.stdUniform.replicateM(envX.bounds.size).map(_.toNel.get),
      _ => Dist.stdUniform.replicateM(envX.bounds.size).map(_.toNel.get))

}

case class MGParticle(id: Int,
                      pb: Position,
                      pos: Position,
                      velocity: Position,
                      swarmID: Int,
                      lambda: Lambda) {
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

  def createCollection(lambda: (Double, EnvironmentX) => Lambda,
                       envX: EnvironmentX): RVar[NonEmptyList[MGParticle]] = {
    val ids =
      envX.cp.swarmSizes.toList.zipWithIndex.flatMap(x =>
        if (x._1 >= 1) (1 to x._1).toList.map(_ => x._2) else List())
    ids.toNel.get.traverse(
      id =>
        Dist.stdUniform.flatMap(
          initLambda =>
            Position
              .createPositionX(envX)
              .map(p => MGParticle(0, p, p, p.zeroed, id, lambda(initLambda, envX)))))
  }

}
