package cilib.research.benchmarks.wfg

import java.lang.Math._

import cilib.research.benchmarks.wfg.Misc._
import scalaz.Scalaz._
import scalaz._

object ShapesFunctions {

  def shape_args_ok(x: NonEmptyList[Double], m: Int): Boolean =
    vector_in_01(x) && m >= 1 && m <= x.size

  def template(x: NonEmptyList[Double],
               m: Int,
               f: (Double, Double) => Double,
               c: Int => Double): Option[Double] =
    Assert(shape_args_ok(x, m)).map { _ =>
      val index = x.size - m
      val result = x.toList.slice(0, index).foldLeft(1.0)(f)
      correct_to_01(
        if (m != 1.0) result * c(index)
        else result
      )
    }

  def linear(x: NonEmptyList[Double], m: Int): Option[Double] =
    template(x, m, (a, c) => a * c, index => 1 - x.toList(index))

  def convex(x: NonEmptyList[Double], m: Int): Option[Double] =
    template(x,
             m,
             (a, c) => a * (1 - cos(c * PI / 2)),
             index => 1 - sin(x.toList.apply(index) * PI / 2))

  def concave(x: NonEmptyList[Double], m: Int): Option[Double] =
    template(x, m, (a, c) => a * sin(c * PI / 2), index => cos(x.toList(index) * PI / 2))

  def mixed(x: NonEmptyList[Double], A: Int, alpha: Double): Option[Double] =
    for {
      _ <- Assert(vector_in_01(x))
      _ <- Assert(x.size != 0)
      _ <- Assert(A >= 1)
      _ <- Assert(alpha > 0.0)
    } yield {
      val tmp = 2 * PI * A
      correct_to_01(pow((1 - x.head) - cos(tmp * x.head + PI / 2) / tmp, alpha))
    }

  def disc(x: NonEmptyList[Double], A: Int, alpha: Int, beta: Int): Option[Double] =
    for {
      _ <- Assert(vector_in_01(x))
      _ <- Assert(x.size != 0)
      _ <- Assert(A >= 1)
      _ <- Assert(alpha > 0.0)
      _ <- Assert(beta > 0.0)
    } yield correct_to_01(1 - pow(x.head, alpha) * pow(cos(A * pow(x.head, beta) * PI), 2.0))

}
