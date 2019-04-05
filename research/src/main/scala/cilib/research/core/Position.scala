package cilib.research.core
import cilib.{Dist, RVar}
import scalaz.Scalaz._
import scalaz._
import spire.math.Interval

case class Position(pos: NonEmptyList[Double],
                     bounds: NonEmptyList[Interval[Double]],
                     fitness: NonEmptyList[Double]) {

  def map(f: Double => Double): Position =
    this.copy(pos = pos.map(f))

  def traverse[G[_]: Applicative](f: Double => G[Double]): G[Position] =
    pos.traverse(f).map(l => this.copy(pos = l))

  def updateFitness(x: NonEmptyList[Double]) =
    this.copy(fitness = x)

  def zeroed: Position =
    this.map(_ => 0.0)

  def +(other: Position): Position =
    this.copy(pos = this.pos.zip(other.pos).map(t => t._1 + t._2))

  def +(other: NonEmptyList[Double]): Position =
    this.copy(pos = this.pos.zip(other).map(t => t._1 + t._2))

  def -(other: Position): Position =
    this.copy(pos = this.pos.zip(other.pos).map(t => t._1 - t._2))

  def *:(scalar: Double): Position =
    this.copy(pos = this.pos.map(_ * scalar))

  def *>:(l: NonEmptyList[Double]): Position =
    this.copy(pos = this.pos.zip(l).map(x => x._1 * x._2))

  def isInbounds: Boolean =
    Foldable1[NonEmptyList].foldLeft(pos.zip(bounds), true)((a, c) => a && c._2.contains(c._1))
}

object Position {
  def createPosition(benchmark: Benchmark): RVar[Position] =
    benchmark.bounds
      .traverse(Dist.uniform)
      .map(x =>
        Position(x, benchmark.bounds, benchmark.f(x))
      )
}
