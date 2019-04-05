package cilib.research.benchmarks.zdt

import java.lang.Math._

import cilib._
import scalaz.Scalaz._
import scalaz._
import spire.implicits._
import spire.math.Interval

sealed abstract class ZDT {
  def assert(list: NonEmptyList[Double], f: NonEmptyList[Double] => Double): Option[Double] = {
    val check = this match {
      case ZDT1 | ZDT2 | ZDT3 | ZDT6 => list.toList.forall(x => x >= 0.0 && x <= 1.0)
      case ZDT4 =>
        (list.head >= 0.0 && list.head <= 1.0) && list.tail.toList.forall(x =>
          x >= -5.0 && x <= 5.0)
    }

    if (check) Some(f(list))
    else None
  }

  def bounds: NonEmptyList[Interval[Double]] =
    this match {
      case ZDT1 | ZDT2 | ZDT3 => Interval(0.0, 1.0) ^ 30
      case ZDT4               => Interval(0.0, 1.0) <:: (Interval(-5.0, 5.0) ^ 9)
      case ZDT6               => Interval(0.0, 1.0) ^ 10

    }

  def g(list: NonEmptyList[Double]): Option[Double] =
    assert(
      list,
      this match {
        case ZDT1 | ZDT2 | ZDT3 =>
          l: NonEmptyList[Double] =>
            1 + (9 * l.tail.suml) / (l.size - 1)
        case ZDT4 =>
          l: NonEmptyList[Double] =>
            1 + 10 * (l.size - 1) + l.tail.foldLeft(0.0)((a, c) =>
              a + (pow(c, 2) - 10 * cos(4 * PI * c)))
        case ZDT6 =>
          l: NonEmptyList[Double] =>
            1 + 9 * pow(l.tail.suml / (l.size - 1), 0.25)
      }
    )

  def h(list: NonEmptyList[Double]): Option[Double] =
    f1(list).flatMap(_f1 =>
      g(list).flatMap(_g =>
        assert(list, this match {
          case ZDT1 | ZDT4 =>
            _ =>
              1 - sqrt(_f1 / _g)
          case ZDT2 | ZDT6 =>
            _ =>
              1 - pow(_f1 / _g, 2)
          case ZDT3 =>
            _ =>
              1 - sqrt(_f1 / _g) - (_f1 / _g) * sin(10 * PI * _f1)
        })))

  def f1(list: NonEmptyList[Double]): Option[Double] =
    assert(list, this match {
      case ZDT1 | ZDT2 | ZDT3 | ZDT4 =>
        l: NonEmptyList[Double] =>
          l.head
      case ZDT6 =>
        l: NonEmptyList[Double] =>
          1 - exp(-4 * l.head) * pow(sin(6 * PI * l.head), 6)
    })

  def f2(list: NonEmptyList[Double]): Option[Double] =
    g(list).flatMap(_g => h(list).map(_h => _g * _h))

  def evaluate(l: NonEmptyList[Double]): Option[NonEmptyList[Double]] =
    for {
      x <- f1(l)
      y <- f2(l)
    } yield NonEmptyList(x, y)
}

case object ZDT1 extends ZDT

case object ZDT2 extends ZDT

case object ZDT3 extends ZDT

case object ZDT4 extends ZDT

case object ZDT6 extends ZDT

object ZDT {

  def ZDT1F(input: NonEmptyList[Double]): NonEmptyList[Double] =
    ZDT1.evaluate(input).get

  def ZDT2F(input: NonEmptyList[Double]): NonEmptyList[Double] =
    ZDT2.evaluate(input).get

  def ZDT3F(input: NonEmptyList[Double]): NonEmptyList[Double] =
    ZDT3.evaluate(input).get

  def ZDT4F(input: NonEmptyList[Double]): NonEmptyList[Double] =
    ZDT4.evaluate(input).get

  def ZDT6F(input: NonEmptyList[Double]): NonEmptyList[Double] =
    ZDT6.evaluate(input).get

}
