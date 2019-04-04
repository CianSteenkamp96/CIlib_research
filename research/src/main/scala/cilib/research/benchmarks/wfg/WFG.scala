package cilib.research.benchmarks.wfg

import cilib.research.core.{ControlParameters, EnvironmentX}
import scalaz.Scalaz._
import scalaz._
import spire.implicits._
import spire.math.Interval

object WFG {
  val bounds = (0 until 30).toList.toNel.get.map(x => Interval(0.0, 2.0 * x + 1.0))

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

  val wfg2objEnvs = NonEmptyList(
    EnvironmentX("WFG1",
                 WFG1(2),
                 bounds,
                 ControlParameters(0.275, 1.65, 1.80, 1.75, NonEmptyList(45, 5))),
    EnvironmentX("WFG2",
                 WFG2(2),
                 bounds,
                 ControlParameters(0.750, 1.15, 1.70, 1.05, NonEmptyList(24, 26))),
    EnvironmentX("WFG3",
                 WFG3(2),
                 bounds,
                 ControlParameters(0.600, 1.60, 1.85, 0.95, NonEmptyList(31, 19))),
    EnvironmentX("WFG4",
                 WFG4(2),
                 bounds,
                 ControlParameters(0.100, 0.80, 1.65, 1.70, NonEmptyList(2, 48))),
    EnvironmentX("WFG5",
                 WFG5(2),
                 bounds,
                 ControlParameters(0.600, 0.80, 1.60, 1.85, NonEmptyList(50, 0))),
    EnvironmentX("WFG6",
                 WFG6(2),
                 bounds,
                 ControlParameters(0.525, 0.65, 0.60, 1.65, NonEmptyList(19, 31))),
    EnvironmentX("WFG7",
                 WFG7(2),
                 bounds,
                 ControlParameters(0.450, 1.20, 1.85, 1.55, NonEmptyList(29, 21))),
    EnvironmentX("WFG8",
                 WFG8(2),
                 bounds,
                 ControlParameters(0.750, 1.00, 1.65, 1.05, NonEmptyList(37, 13))),
    EnvironmentX("WFG9",
                 WFG9(2),
                 bounds,
                 ControlParameters(0.275, 1.00, 0.50, 1.70, NonEmptyList(13, 37)))
  )

  val wfg3objEnvs = NonEmptyList(
    EnvironmentX("WFG1",
                 WFG1(3),
                 bounds,
                 ControlParameters(0.125, 1.20, 1.30, 1.75, NonEmptyList(37, 4, 9))),
    EnvironmentX("WFG2",
                 WFG2(3),
                 bounds,
                 ControlParameters(0.275, 1.25, 1.40, 1.70, NonEmptyList(24, 25, 1))),
    EnvironmentX("WFG3",
                 WFG3(3),
                 bounds,
                 ControlParameters(0.525, 1.65, 1.75, 0.75, NonEmptyList(29, 10, 11))),
    EnvironmentX("WFG4",
                 WFG4(3),
                 bounds,
                 ControlParameters(0.275, 1.75, 0.50, 1.05, NonEmptyList(29, 21, 0))),
    EnvironmentX("WFG5",
                 WFG5(3),
                 bounds,
                 ControlParameters(0.575, 0.60, 1.85, 1.75, NonEmptyList(2, 48, 0))),
    EnvironmentX("WFG6",
                 WFG6(3),
                 bounds,
                 ControlParameters(0.300, 0.90, 0.90, 1.90, NonEmptyList(5, 30, 15))),
    EnvironmentX("WFG7",
                 WFG7(3),
                 bounds,
                 ControlParameters(0.425, 1.45, 1.50, 1.40, NonEmptyList(10, 22, 18))),
    EnvironmentX("WFG8",
                 WFG8(3),
                 bounds,
                 ControlParameters(0.425, 0.95, 1.75, 1.85, NonEmptyList(4, 23, 23))),
    EnvironmentX("WFG9",
                 WFG9(3),
                 bounds,
                 ControlParameters(0.275, 1.25, 0.75, 1.50, NonEmptyList(4, 45, 1)))
  )

}
