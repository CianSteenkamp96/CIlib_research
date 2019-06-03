// Cian Steenkamp
// Based on jMetal implementation of DTLZ problems/functions

package cilib.research.benchmarks.dtlz

import scalaz.Scalaz._
import scalaz._
import spire.implicits._
import spire.math.Interval

sealed abstract class DTLZ {
  def calcF(z: NonEmptyList[Double], g: Double, m: Int): IndexedSeq[Double] =
    this match {
      case DTLZ1 =>
        (0 until m).map(i => {
          val aux = m - (i + 1)
          val rslt = z.zipWithIndex.foldLeft((1.0 + g) * 0.5)(
            (acc, next) =>
              if (next._2 < aux)
                acc * next._1
              else
                acc
          )
          if (i != 0)
            rslt * (1 - z.toList(aux))
          else
            rslt
        })
      case DTLZ2 | DTLZ3 =>
        (0 until m).map(i => {
          val aux = m - (i + 1)
          val rslt = z.zipWithIndex.foldLeft(1.0 + g)(
            (acc, next) =>
              if (next._2 < aux)
                acc * Math.cos(next._1 * 0.5 * Math.PI)
              else
                acc
          )
          if (i != 0)
            rslt * Math.sin(z.toList(aux) * 0.5 * Math.PI)
          else
            rslt
        })
      case DTLZ4 =>
        (0 until m).map(i => {
          val aux = m - (i + 1)
          val rslt = z.zipWithIndex.foldLeft(1.0 + g)(
            (acc, next) =>
              if (next._2 < aux)
                acc * Math.cos(Math.pow(next._1, DTLZ4.alpha)) * (Math.PI / 2.0)
              else
                acc
          )
          if (i != 0)
            rslt * Math.cos(Math.pow(z.toList(aux), DTLZ4.alpha)) * (Math.PI / 2.0)
          else
            rslt
        })
      case DTLZ5 | DTLZ6 =>
        val t = Math.PI / (4.0 * (1.0 + g))
        val theta = (0 until m).map(
          i =>
            if (i == 0)
              z.head * Math.PI / 2.0
            else
              t * (1.0 + 2.0 * g * z.toList(i)))
        (0 until m).map(i => {
          val aux = m - (i + 1)
          val rslt = z.zipWithIndex.foldLeft(1.0 + g)(
            (acc, next) =>
              if (next._2 < aux)
                acc * Math.cos(theta(next._2))
              else
                acc
          )
          if (i != 0)
            rslt * Math.sin(theta(aux))
          else
            rslt
        })
      case DTLZ7 =>
        (0 until m).map(
          i =>
            if (i < (m - 1))
              z.toList(i)
            else
              (1 + g) * z.foldLeft(0.0)((acc, next) =>
                acc + (next / (1.0 + g)) * (1 + Math.sin(3.0 * Math.PI * next))))
    }

  def calcG(k: Int, nel: NonEmptyList[Double]): Double =
    this match {
      case DTLZ1 | DTLZ3 =>
        nel.zipWithIndex.foldLeft(0.0) { (acc, element) =>
          if (element._2 >= nel.size - k)
            acc + ((element._1 - 0.5) * (element._1 - 0.5) - Math.cos(
              20.0 * Math.PI * (element._1 - 0.5)))
          else
            acc
        }
      case DTLZ2 | DTLZ4 | DTLZ5 =>
        nel.zipWithIndex.foldLeft(0.0) { (acc, element) =>
          if (element._2 >= nel.size - k)
            acc + ((element._1 - 0.5) * (element._1 - 0.5))
          else
            acc
        }
      case DTLZ6 =>
        nel.zipWithIndex.foldLeft(0.0) { (acc, element) =>
          if (element._2 >= nel.size - k)
            acc + Math.pow(element._1, 0.1)
          else
            acc
        }
      case DTLZ7 =>
        nel.zipWithIndex.foldLeft(0.0) { (acc, element) =>
          if (element._2 >= nel.size - k)
            acc + element._1
          else
            acc
        }
    }

  def dtlz(z: NonEmptyList[Double],
           numObjectives: Int,
           numDimensions: Int): NonEmptyList[Double] = {
    val k = numDimensions - numObjectives + 1
    this match {
      case DTLZ1 | DTLZ3                 => calcF(z, 100.0 * (k + calcG(k, z)), numObjectives).toList.toNel.get
      case DTLZ2 | DTLZ4 | DTLZ5 | DTLZ6 => calcF(z, calcG(k, z), numObjectives).toList.toNel.get
      case DTLZ7                         => calcF(z, 1 + (9.0 * calcG(k, z)) / k, numObjectives).toList.toNel.get
    }
  }
}

case object DTLZ1 extends DTLZ
case object DTLZ2 extends DTLZ
case object DTLZ3 extends DTLZ
case object DTLZ4 extends DTLZ { val alpha = 100.0 }
case object DTLZ5 extends DTLZ
case object DTLZ6 extends DTLZ
case object DTLZ7 extends DTLZ

object DTLZ {
  val bounds: NonEmptyList[Interval[Double]] = (0 until 30).toList.toNel.get.map(x => Interval(0.0, 1.0))

  def DTLZ1F(m: Int)(nel: NonEmptyList[Double]): NonEmptyList[Double] =
    DTLZ1.dtlz(nel, m, bounds.size)

  def DTLZ2F(m: Int)(nel: NonEmptyList[Double]): NonEmptyList[Double] =
    DTLZ2.dtlz(nel, m, bounds.size)

  def DTLZ3F(m: Int)(nel: NonEmptyList[Double]): NonEmptyList[Double] =
    DTLZ3.dtlz(nel, m, bounds.size)

  def DTLZ4F(m: Int)(nel: NonEmptyList[Double]): NonEmptyList[Double] =
    DTLZ4.dtlz(nel, m, bounds.size)

  def DTLZ5F(m: Int)(nel: NonEmptyList[Double]): NonEmptyList[Double] =
    DTLZ5.dtlz(nel, m, bounds.size)

  def DTLZ6F(m: Int)(nel: NonEmptyList[Double]): NonEmptyList[Double] =
    DTLZ6.dtlz(nel, m, bounds.size)

  def DTLZ7F(m: Int)(nel: NonEmptyList[Double]): NonEmptyList[Double] =
    DTLZ7.dtlz(nel, m, bounds.size)
}
