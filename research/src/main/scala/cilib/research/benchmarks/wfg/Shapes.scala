package cilib.research.benchmarks.wfg

import cilib.research.benchmarks.wfg.FrameworkFunctions._
import cilib.research.benchmarks.wfg.Misc._
import cilib.research.benchmarks.wfg.ShapesFunctions._
import scalaz.Scalaz._
import scalaz._

object Shapes {

  def WFG_create_A(M: Int, degenerate: Boolean) =
    for {
      _ <- Assert(M >= 2)
      list <- (1 until M).toList
        .map(x => {
          if (degenerate)
            if (x == 1) 1.0
            else 0.0
          else 1.0
        })
        .toNel
    } yield list

  def WFG_calculate_f(x: NonEmptyList[Double], h: NonEmptyList[Double]) =
    for {
      _ <- Assert(x.size == h.size)
      _ <- Assert(vector_in_01(x))
      _ <- Assert(vector_in_01(h))
      list <- (1 to h.size).toList.map(i => i * 2.0).toNel
      result <- calculate_f(1.0, x, h, list)
    } yield result

  def template(t_p: NonEmptyList[Double],
               range: Int => List[Int],
               shape: (NonEmptyList[Double], Int) => Option[Double],
               f: (NonEmptyList[Double], NonEmptyList[Double]) => Option[NonEmptyList[Double]])
    : Option[NonEmptyList[Double]] =
    for {
      _ <- Assert(vector_in_01(t_p))
      _ <- Assert(t_p.size >= 2)
      index = t_p.size
      a <- WFG_create_A(index, false)
      x <- calculate_x(t_p, a)
      list <- range(index).traverse(n => shape(x, n))
      nel <- list.toNel
      result <- f(x, nel)
    } yield result

  def WFG1_shape(t_p: NonEmptyList[Double]) =
    template(t_p,
             x => (1 until x).toList,
             convex,
             (x, nel) => WFG_calculate_f(x, nel.append(NonEmptyList(mixed(x, 5, 1.0).get))))

  def WFG2_shape(t_p: NonEmptyList[Double]) =
    template(t_p,
             x => (1 until x).toList,
             convex,
             (x, nel) => WFG_calculate_f(x, nel.append(NonEmptyList(disc(x, 5, 1, 1).get))))

  def WFG3_shape(t_p: NonEmptyList[Double]) =
    template(t_p, x => (1 to x).toList, linear, (x, nel) => WFG_calculate_f(x, nel))

  def WFG4_shape(t_p: NonEmptyList[Double]) =
    template(t_p, x => (1 to x).toList, concave, (x, nel) => WFG_calculate_f(x, nel))

  def I1_shape(t_p: NonEmptyList[Double]) =
    template(t_p,
             x => (1 to x).toList,
             concave,
             (x, nel) => calculate_f(1.0, x, nel, t_p.map(_ => 1)))

}
