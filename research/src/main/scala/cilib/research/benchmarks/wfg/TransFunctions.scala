package cilib.research.benchmarks.wfg

import java.lang.Math._

import cilib._
import cilib.research.benchmarks.wfg.Misc._
import scalaz.Scalaz._
import scalaz._

object TransFunctions {
  def b_poly(y: Double, alpha: Double): Option[Double] =
    for {
      _ <- Assert(y >= 0.0)
      _ <- Assert(y <= 1.0)
      _ <- Assert(alpha > 0.0)
      _ <- Assert(alpha != 1.0)
    } yield correct_to_01(pow(y, alpha))

  def b_flat(y: Double, A: Double, B: Double, C: Double): Option[Double] =
    for {
      _ <- Assert(y >= 0.0)
      _ <- Assert(y <= 1.0)
      _ <- Assert(A >= 0.0)
      _ <- Assert(A <= 1.0)
      _ <- Assert(B >= 0.0)
      _ <- Assert(B <= 1.0)
      _ <- Assert(C >= 0.0)
      _ <- Assert(C <= 1.0)
      _ <- Assert(B < C)
      _ <- Assert(B != 0.0 || A == 0.0)
      _ <- Assert(B != 0.0 || C != 1.0)
      _ <- Assert(C != 1.0 || A == 1.0)
      _ <- Assert(C != 1.0 || B != 0.0)
    } yield {
      val tmp1 = min(0.0, floor(y - B)) * A * (B - y) / B
      val tmp2 = min(0.0, floor(C - y)) * (1.0 - A) * (y - C) / (1.0 - C)
      correct_to_01(A + tmp1 - tmp2)
    }

  def b_param(y: Double, u: Double, A: Double, B: Double, C: Double): Option[Double] =
    for {
      _ <- Assert(y >= 0.0)
      _ <- Assert(y <= 1.0)
      _ <- Assert(u >= 0.0)
      _ <- Assert(u <= 1.0)
      _ <- Assert(A > 0.0)
      _ <- Assert(A < 1.0)
      _ <- Assert(B > 0.0)
      _ <- Assert(B < C)
    } yield {
      val v = A - (1.0 - 2.0 * u) * abs(floor(0.5 - u) + A)
      correct_to_01(pow(y, B + (C - B) * v))
    }

  def s_linear(y: Double, A: Double): Option[Double] =
    for {
      _ <- Assert(y >= 0.0)
      _ <- Assert(y <= 1.0)
      _ <- Assert(A > 0.0)
      _ <- Assert(A < 1.0)
    } yield correct_to_01(abs(y - A) / abs(floor(A - y) + A))

  def s_decept(y: Double, A: Double, B: Double, C: Double): Option[Double] =
    for {
      _ <- Assert(y >= 0.0)
      _ <- Assert(y <= 1.0)
      _ <- Assert(A > 0.0)
      _ <- Assert(A < 1.0)
      _ <- Assert(B > 0.0)
      _ <- Assert(B < 1.0)
      _ <- Assert(C > 0.0)
      _ <- Assert(C < 1.0)
      _ <- Assert(A - B > 0.0)
      _ <- Assert(A + B < 1.0)
    } yield {
      val tmp1 = floor(y - A + B) * (1.0 - C + (A - B) / B) / (A - B)
      val tmp2 = floor(A + B - y) * (1.0 - C + (1.0 - A - B) / B) / (1.0 - A - B)
      correct_to_01(1.0 + (abs(y - A) - B) * (tmp1 + tmp2 + 1.0 / B))
    }

  def s_multi(y: Double, A: Int, B: Double, C: Double): Option[Double] =
    for {
      _ <- Assert(y >= 0.0)
      _ <- Assert(y <= 1.0)
      _ <- Assert(A >= 1)
      _ <- Assert(B >= 0.0)
      _ <- Assert((4.0 * A + 2.0) * PI >= 4.0 * B)
      _ <- Assert(C > 0.0)
      _ <- Assert(C < 1.0)
    } yield {
      val tmp1 = abs(y - C) / (2.0 * (floor(C - y) + C))
      val tmp2 = (4.0 * A + 2.0) * PI * (0.5 - tmp1)
      correct_to_01((1.0 + cos(tmp2) + 4.0 * B * pow(tmp1, 2.0)) / (B + 2.0))
    }

  def r_sum(y: NonEmptyList[Double], w: NonEmptyList[Double]): Option[Double] =
    for {
      _ <- Assert(y.size != 0)
      _ <- Assert(w.size == y.size)
      _ <- Assert(vector_in_01(y))
      _ <- w.traverse(x => Assert(x > 0.0))
    } yield {
      val numerator = y.zip(w).map(x => x._1 * x._2).suml
      val denominator = w.suml
      correct_to_01(numerator / denominator)
    }

  def r_nonsep(y: NonEmptyList[Double], A: Int): Option[Double] =
    for {
      _ <- Assert(y.size != 0)
      _ <- Assert(vector_in_01(y))
      _ <- Assert(A >= 1)
      _ <- Assert(A <= y.size)
      _ <- Assert(y.size % A == 0)
    } yield {
      val list = y.toList
      val sliceIndices = list.slice(0, A - 1).indices.toList
      val numerator = list.zipWithIndex
        .map(x => x._1 + sliceIndices.map(i => abs(x._1 - list((x._2 + i + 1) % list.size))).suml)
        .suml
      val tmp = ceil(A / 2.0)
      val denominator = y.size * tmp * (1.0 + 2 * A - 2 * tmp) / A
      numerator / denominator
    }
}
