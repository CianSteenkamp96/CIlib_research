////////////////////////////////////////////// NEW - Bounds moved //////////////////////////////////////////////
package cilib.research.benchmarks.wfg

import scalaz._

object WFG {

  def roundAt(p: Int)(n: Double): Double = {
    val s = math.pow(10, p)
    math.round(n * s) / s
  }

  def calcK(M: Int): Int = if (M == 2) 4 else 2 * (M - 1)

  def WFG1(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG1(z, calcK(M), M).get

  def WFG2(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG2(z, calcK(M), M).get

  def WFG3(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG3(z, calcK(M), M).get

  def WFG4(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG4(z, calcK(M), M).get

  def WFG5(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG5(z, calcK(M), M).get

  def WFG6(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG6(z, calcK(M), M).get

  def WFG7(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG7(z, calcK(M), M).get

  def WFG8(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG8(z, calcK(M), M).get

  def WFG9(M: Int)(z: NonEmptyList[Double]): NonEmptyList[Double] =
    Problems.WFG9(z, calcK(M), M).get

}
