package cilib.research.mgpso

import cilib.research.core._
import cilib.{Dist, RVar}
import scalaz.Scalaz._
import scalaz._

case class LambdaStrategy(value: RVar[NonEmptyList[Double]],
                          next: RVar[NonEmptyList[Double]] => RVar[NonEmptyList[Double]]) {
  def update: LambdaStrategy =
    LambdaStrategy(next(value), next)
}






object LambdaStrategy {

  def linearIncreasing(envX: Benchmark): LambdaStrategy =
    new LambdaStrategy(RVar.pure(List.fill(envX.bounds.size)(0.0).toNel.get),
      rl => rl.map(_.map(x => x + 0.0005)))

  def linearDecreasing(envX: Benchmark): LambdaStrategy =
    new LambdaStrategy(RVar.pure(List.fill(envX.bounds.size)(1.0).toNel.get),
      rl => rl.map(_.map(x => x - 0.0005)))

  def std(envX: Benchmark): LambdaStrategy =
    new LambdaStrategy(RVar.pure(List.fill(envX.bounds.size)(x).toNel.get), rl => rl)

  def random(envX: Benchmark): LambdaStrategy =
    new LambdaStrategy({
      val value = RList.getHeadAsList(envX).toNel.get
      RList.drop
      RVar.pure(value)
    }, _ => {
      val value = RList.getHeadAsList(envX).toNel.get
      RList.drop
      RVar.pure(value)
    })

  def random_i(envX: Benchmark): LambdaStrategy =
    new LambdaStrategy(Dist.stdUniform.map(value => List.fill(envX.bounds.size)(value).toNel.get),
      _ => Dist.stdUniform.map(value => List.fill(envX.bounds.size)(value).toNel.get))

  def random_i_j(envX: Benchmark): LambdaStrategy =
    new LambdaStrategy(Dist.stdUniform.replicateM(envX.bounds.size).map(_.toNel.get),
      _ => Dist.stdUniform.replicateM(envX.bounds.size).map(_.toNel.get))

}
