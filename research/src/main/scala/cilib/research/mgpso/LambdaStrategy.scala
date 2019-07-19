package cilib.research.mgpso

import cilib.{Dist, RNG, RVar}
import scalaz.Scalaz._
import scalaz._
import spire.math.Interval

case class LambdaStrategy(name: String,
                          value: RVar[NonEmptyList[Double]],
                          next: RVar[NonEmptyList[Double]] => RVar[NonEmptyList[Double]],
                          state: List[Double]) {

  def update: LambdaStrategy =
    name match {
      case "R" => LambdaStrategy(name, value.map(l => l.map(_ => state.head)), next, state.tail)
      case _   => LambdaStrategy(name, next(value), next, state)
    }

  def evalValue(rng: RNG): LambdaStrategy =
    name match {
      case "R"   => this.copy(state = Dist.stdUniform.replicateM(3000).eval(rng))
      case _     => this
    }


  def setValue(x: Double): LambdaStrategy =
    name match {
      case "STD" => this.copy(value = value.map(l => l.map(_ => x)))
      case _ => this
    }

}

object LambdaStrategy {

  def Standard(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("STD",
                   Dist.stdUniform.map(x => List.fill(bounds.size)(x).toNel.get),
                   value => value,
                   List())

  def LinearIncreasing(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("LI",
                   RVar.pure(List.fill(bounds.size)(0.0).toNel.get),
                   rl => rl.map(_.map(x => x + 0.0005)),
                   List())

  def LinearDecreasing(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("LD",
                   RVar.pure(List.fill(bounds.size)(1.0).toNel.get),
                   value => value.map(_.map(x => x - 0.0005)),
                   List())

  def Random(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("R",
                   Dist.stdUniform.map(value => List.fill(bounds.size)(value).toNel.get),
                   value => value,
                   List())

  def RandomI(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("RI",
                   Dist.stdUniform.map(value => List.fill(bounds.size)(value).toNel.get),
                   value => value,
                   List())

  def RandomIJ(bounds: NonEmptyList[Interval[Double]]): LambdaStrategy =
    LambdaStrategy("RIJ",
                   Dist.stdUniform.replicateM(bounds.size).map(_.toNel.get),
                   value => value,
                   List())

}
