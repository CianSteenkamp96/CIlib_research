package cilib.research.benchmarks.wfg

import cilib.research.benchmarks.wfg.Shapes._
import cilib.research.benchmarks.wfg.Transitions._
import scalaz.Scalaz._
import scalaz._

object Problems {

  def ArgsOK(z: NonEmptyList[Double], k: Int, M: Int) =
    k >= 1 && k < z.size && M >= 2 && k % (M - 1) == 0

  def WFG_normalise_z(z: NonEmptyList[Double]) =
    z.zipWithIndex.traverse(x => {
      val bound = 2.0 * (x._2 + 1)
      Assert(x._1 >= 0.0).flatMap(_ => Assert(x._1 <= bound).map(_ => x._1 / bound))
    })

  private def WFGTemplate(z: NonEmptyList[Double], k: Int, M: Int): Option[NonEmptyList[Double]] =
    Assert(ArgsOK(z, k, M)).flatMap(_ => WFG_normalise_z(z))

  private def ITemplate(
      z: NonEmptyList[Double],
      k: Int,
      M: Int,
      f: (NonEmptyList[Double]) => Option[NonEmptyList[Double]]): Option[NonEmptyList[Double]] =
    for {
      _ <- Assert(ArgsOK(z, k, M))
      unitIntervals <- f(z)
      result <- I1_shape(unitIntervals)
    } yield result

  def WFG1(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      y <- WFGTemplate(z, k, M)
      step1 <- WFG1_t1(y, k)
      step2 <- WFG1_t2(step1, k)
      step3 <- WFG1_t3(step2)
      step4 <- WFG1_t4(step3, k, M)
      result <- WFG1_shape(step4)
    } yield result

  def WFG2(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      _ <- Assert(ArgsOK(z, k, M))
      _ <- Assert((z.size - k) % 2 == 0)
      y <- WFG_normalise_z(z)
      step1 <- WFG1_t1(y, k)
      step2 <- WFG2_t2(step1, k)
      step3 <- WFG2_t3(step2, k, M)
      result <- WFG2_shape(step3)
    } yield result

  def WFG3(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      _ <- Assert(ArgsOK(z, k, M))
      _ <- Assert((k - z.size) % 2 == 0)
      y <- WFG_normalise_z(z)
      step1 <- WFG1_t1(y, k)
      step2 <- WFG2_t2(step1, k)
      step3 <- WFG2_t3(step2, k, M)
      result <- WFG3_shape(step3)
    } yield result

  def WFG4(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      y <- WFGTemplate(z, k, M)
      step1 <- WFG4_t1(y)
      step2 <- WFG2_t3(step1, k, M)
      result <- WFG4_shape(step2)
    } yield result

  def WFG5(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      y <- WFGTemplate(z, k, M)
      step1 <- WFG5_t1(y)
      step2 <- WFG2_t3(step1, k, M)
      result <- WFG4_shape(step2)
    } yield result

  def WFG6(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      y <- WFGTemplate(z, k, M)
      step1 <- WFG1_t1(y, k)
      step2 <- WFG6_t2(step1, k, M)
      result <- WFG4_shape(step2)
    } yield result

  def WFG7(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      y <- WFGTemplate(z, k, M)
      step1 <- WFG7_t1(y, k)
      step2 <- WFG1_t1(step1, k)
      step3 <- WFG2_t3(step2, k, M)
      result <- WFG4_shape(step3)
    } yield result

  def WFG8(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      y <- WFGTemplate(z, k, M)
      step1 <- WFG8_t1(y, k)
      step2 <- WFG1_t1(step1, k)
      step3 <- WFG2_t3(step2, k, M)
      result <- WFG4_shape(step3)
    } yield result

  def WFG9(z: NonEmptyList[Double], k: Int, M: Int) =
    for {
      y <- WFGTemplate(z, k, M)
      step1 <- WFG9_t1(y)
      step2 <- WFG9_t2(step1, k)
      step3 <- WFG6_t2(step2, k, M)
      result <- WFG4_shape(step3)
    } yield result

  def I1(z: NonEmptyList[Double], k: Int, M: Int) =
    ITemplate(z,
              k,
              M,
              y =>
                for {
                  step1 <- I1_t2(y, k)
                  step2 <- I1_t3(step1, k, M)
                } yield step2)

  def I2(z: NonEmptyList[Double], k: Int, M: Int) =
    ITemplate(z,
              k,
              M,
              y =>
                for {
                  step1 <- I2_t1(y)
                  step2 <- I1_t2(step1, k)
                  step3 <- I1_t3(step2, k, M)
                } yield step3)

  def I3(z: NonEmptyList[Double], k: Int, M: Int) =
    ITemplate(z,
              k,
              M,
              y =>
                for {
                  step1 <- I3_t1(y)
                  step2 <- I1_t2(step1, k)
                  step3 <- I1_t3(step2, k, M)
                } yield step3)

  def I4(z: NonEmptyList[Double], k: Int, M: Int) =
    ITemplate(z,
              k,
              M,
              y =>
                for {
                  step1 <- I1_t2(y, k)
                  step2 <- I4_t3(step1, k, M)
                } yield step2)

  def I5(z: NonEmptyList[Double], k: Int, M: Int) =
    ITemplate(z,
              k,
              M,
              y =>
                for {
                  step1 <- I3_t1(y)
                  step2 <- I1_t2(step1, k)
                  step3 <- I4_t3(step2, k, M)
                } yield step3)
}
