package cilib.research.benchmarks.wfg

import java.lang.Math._

import cilib.research.benchmarks.wfg.Misc._
import scalaz.Scalaz._
import scalaz._

object FrameworkFunctions {

  def normalise_z(z: NonEmptyList[Double], z_max: NonEmptyList[Double]) =
    z.zip(z_max)
      .traverse(x => {
        for {
          _ <- Assert(x._1 >= 0.0)
          _ <- Assert(x._1 <= x._2)
          _ <- Assert(x._2 > 0.0)
        } yield x._1 / x._2
      })

  def calculate_x(t_p: NonEmptyList[Double], A: NonEmptyList[Double]) =
    for {
      _ <- Assert(vector_in_01(t_p))
      _ <- Assert(t_p.size != 0)
      _ <- Assert(A.size == t_p.size - 1)
    } yield {
      val back = t_p.toList.reverse.head
      t_p
        .zip(A)
        .map(x => max(back, x._2) * (x._1 - 0.5) + 0.5)
        .toNel
        .append(NonEmptyList(back))
    }

  def calculate_f(D: Double,
                  x: NonEmptyList[Double],
                  h: NonEmptyList[Double],
                  s: NonEmptyList[Double]) =
    for {
      _ <- Assert(D > 0.0)
      _ <- Assert(vector_in_01(x))
      _ <- Assert(vector_in_01(h))
      _ <- Assert(x.size == h.size)
      _ <- Assert(h.size == s.size)
    } yield {
      s.zip(h).map(t => D * x.toList.reverse.head + t._1 * t._2)
    }

}
