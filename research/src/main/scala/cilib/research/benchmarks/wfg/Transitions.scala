package cilib.research.benchmarks.wfg

import cilib.research.benchmarks.wfg.Misc._
import cilib.research.benchmarks.wfg.TransFunctions._
import scalaz.Scalaz._
import scalaz._

object Transitions {

  def calc(y: NonEmptyList[Double], w: NonEmptyList[Double], head: Int, tail: Int) =
    subvector(y, head, tail).flatMap(suby =>
      subvector(w, head, tail).flatMap(subw => {
        r_sum(suby, subw)
      }))

  def subvector(v: NonEmptyList[Double], head: Int, tail: Int) =
    for {
      _ <- Assert(head >= 0)
      _ <- Assert(head < tail)
      _ <- Assert(tail <= v.size)
      nel <- v.toList.slice(head, tail).toNel
    } yield nel

  def WFG1_t1(y: NonEmptyList[Double], k: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      suby <- subvector(y, 0, k)
      temp <- subvector(y, k, y.size)
      result <- temp.traverse(x => s_linear(x, 0.35))
    } yield suby.append(result)

  def WFG1_t2(y: NonEmptyList[Double], k: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      suby <- subvector(y, 0, k)
      temp <- subvector(y, k, y.size)
      result <- temp.traverse(x => b_flat(x, 0.8, 0.75, 0.85))
    } yield suby.append(result)

  def WFG1_t3(y: NonEmptyList[Double]) =
    for {
      _ <- Assert(vector_in_01(y))
      result <- y.traverse(x => b_poly(x, 0.02))
    } yield result

  def WFG1_t4(y: NonEmptyList[Double], k: Int, M: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      _ <- Assert(M >= 2)
      _ <- Assert(k % (M - 1) == 0)
      w <- (1 to y.size).toList.map(x => x * 2.0).toNel
      temp <- (1 until M).toList.traverse(i => {
        val head = (i - 1) * k / (M - 1)
        val tail = i * k / (M - 1)
        calc(y, w, head, tail)
      })
      nel <- temp.toNel
      finaX <- calc(y, w, k, y.size)
    } yield nel.append(NonEmptyList(finaX))

  def WFG2_t2(y: NonEmptyList[Double], k: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      _ <- Assert((y.size - k) % 2 == 0)
      t <- subvector(y, 0, k)
      temp <- (k + 1 to k + ((y.size - k) / 2)).toList.traverse(i => {
        val head = k + 2 * (i - k) - 2
        val tail = k + 2 * (i - k)
        subvector(y, head, tail).flatMap(sub => r_nonsep(sub, 2))
      })
      nel <- temp.toNel
    } yield t.append(nel)

  def WFG2_t3(y: NonEmptyList[Double], k: Int, M: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      _ <- Assert(M >= 2)
      _ <- Assert(k % (M - 1) == 0)
      w = y.map(_ => 1.0)
      t <- (1 until M).toList.traverse(i => {
        val head = (i - 1) * k / (M - 1)
        val tail = i * k / (M - 1)
        calc(y, w, head, tail)
      })
      nel <- t.toNel
      finaX <- calc(y, w, k, y.size)
    } yield nel.append(NonEmptyList(finaX))

  def WFG4_t1(y: NonEmptyList[Double]) =
    for {
      _ <- Assert(vector_in_01(y))
      t <- y.traverse(x => s_multi(x, 30, 10, 0.35))
    } yield t

  def WFG5_t1(y: NonEmptyList[Double]) =
    for {
      _ <- Assert(vector_in_01(y))
      t <- y.traverse(x => s_decept(x, 0.35, 0.001, 0.05))
    } yield t

  def WFG6_t2(y: NonEmptyList[Double], k: Int, M: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      _ <- Assert(M >= 2)
      _ <- Assert(k % (M - 1) == 0)
      t <- (1 until M).toList.traverse(i => {
        val head = (i - 1) * k / (M - 1)
        val tail = i * k / (M - 1)
        subvector(y, head, tail).flatMap(sub => r_nonsep(sub, k / (M - 1)))
      })
      nel <- t.toNel
      result <- subvector(y, k, y.size).flatMap(sub => r_nonsep(sub, y.size - k))
    } yield nel.append(NonEmptyList(result))

  def WFG7_t1(y: NonEmptyList[Double], k: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      w = y.map(_ => 1.0)
      t <- (0 until k).toList.traverse(i =>
        subvector(y, i + 1, y.size).flatMap(y_sub =>
          subvector(w, i + 1, y.size).flatMap(w_sub =>
            r_sum(y_sub, w_sub).flatMap(u => b_param(y.toList(i), u, 0.98 / 49.98, 0.02, 50)))))
      nel <- t.toNel
      end <- subvector(y, k, y.size)
    } yield nel.append(end)

  def WFG8_t1(y: NonEmptyList[Double], k: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      w = y.map(_ => 1.0)
      t <- subvector(y, 0, k)
      res <- (k until y.size).toList.traverse(i =>
        subvector(y, 0, i).flatMap(y_sub =>
          subvector(w, 0, i).flatMap(w_sub =>
            r_sum(y_sub, w_sub).flatMap(u => b_param(y.toList(i), u, 0.98 / 49.98, 0.02, 50)))))
      nel <- res.toNel
    } yield t.append(nel)

  def WFG9_t1(y: NonEmptyList[Double]) =
    for {
      _ <- Assert(vector_in_01(y))
      w = y.map(_ => 1.0)
      t <- (0 until y.size - 1).toList.traverse(i =>
        subvector(y, i + 1, y.size).flatMap(y_sub =>
          subvector(w, i + 1, y.size).flatMap(w_sub =>
            r_sum(y_sub, w_sub).flatMap(u => b_param(y.toList(i), u, 0.98 / 49.98, 0.02, 50)))))
      nel <- t.toNel
    } yield nel.append(NonEmptyList(y.last))

  def WFG9_t2(y: NonEmptyList[Double], k: Int) =
    for {
      _ <- Assert(vector_in_01(y))
      _ <- Assert(k >= 1)
      _ <- Assert(k < y.size)
      temp1 <- subvector(y, 0, k)
      a <- temp1.traverse(x => s_decept(x, 0.35, 0.001, 0.05))
      temp2 <- subvector(y, k, y.size)
      b <- temp2.traverse(x => s_multi(x, 30, 95, 0.35))
    } yield a.append(b)

  def I1_t2(y: NonEmptyList[Double], k: Int) =
    WFG1_t1(y, k)

  def I1_t3(y: NonEmptyList[Double], k: Int, M: Int) =
    WFG2_t3(y, k, M)

  def I2_t1(y: NonEmptyList[Double]) =
    WFG9_t1(y)

  def I3_t1(y: NonEmptyList[Double]) =
    for {
      _ <- Assert(vector_in_01(y))
      w = y.map(_ => 1.0)
      t <- (1 until y.size).toList.traverse(i =>
        subvector(y, 0, i).flatMap(y_sub =>
          subvector(w, 0, i).flatMap(w_sub =>
            r_sum(y_sub, w_sub).flatMap(u => b_param(y.toList(i), u, 0.98 / 49.98, 0.02, 50)))))
      nel <- t.toNel
    } yield NonEmptyList(y.head).append(nel)

  def I4_t3(y: NonEmptyList[Double], k: Int, M: Int) =
    WFG6_t2(y, k, M)

}
