package cilib.research.mgpso

import cilib.{Dist, RVar}
import scalaz.Scalaz._
import scalaz._
import spire.math.Interval

case class LambdaStrategy(name: String,
                          value: RVar[NonEmptyList[Double]],
                          next: RVar[NonEmptyList[Double]] => RVar[NonEmptyList[Double]]) {

  def update: LambdaStrategy =
    LambdaStrategy(name, next(value), next)

}

object LambdaStrategy {

  def Standard(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("STD",
                   Dist.stdUniform.map(x => List.fill(bounds.size)(x).toNel.get),
                   value => value)

  def LinearIncreasing(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("LI",
                   RVar.pure(List.fill(bounds.size)(0.0).toNel.get),
                   rl => rl.map(_.map(x => x + 0.0005)))

  def LinearDecreasing(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("LD",
                   RVar.pure(List.fill(bounds.size)(1.0).toNel.get),
                   value => value.map(_.map(x => x - 0.0005)))

  def Random(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy(
      "R", {
        val value = RList.getHeadAsList(bounds.size).toNel.get
        RList.drop
        RVar.pure(value)
      },
      _ => {
        val value = RList.getHeadAsList(bounds.size).toNel.get
        RList.drop
        RVar.pure(value)
      }
    )

  def RandomI(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("RI",
                   Dist.stdUniform.map(value => List.fill(bounds.size)(value).toNel.get),
                   _ => Dist.stdUniform.map(value => List.fill(bounds.size)(value).toNel.get))

  def RandomIJ(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("RIJ",
                   Dist.stdUniform.replicateM(bounds.size).map(_.toNel.get),
                   _ => Dist.stdUniform.replicateM(bounds.size).map(_.toNel.get))

}
