package cilib
package wfg

import Problems._
import scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

object WFGTest extends SafeApp {

  override val runc: IO[Unit] = {

    val z1 = NonEmptyList(0.23, 0.4, 0.45, 0.74, 0.85, 0.99)
    val k1 = 3
    val m1 = 2

    val z2 = NonEmptyList(0.457, 0.963, 0.821, 0.143, 0.111, 0.22)
    val k2 = 2
    val m2 = 3

    for {
      _ <- putStrLn("WFG1")
      _ <- putStrLn(WFG1(z1, k1, m1).toString)
      _ <- putStrLn(WFG1(z2, k2, m2).toString)
      _ <- putStrLn("WFG2")
      _ <- putStrLn(WFG2(z1, k1 + 1, m1).toString)
      _ <- putStrLn(WFG2(z2, k2, m2).toString)
      _ <- putStrLn("WFG3")
      _ <- putStrLn(WFG3(z1, k1 + 1, m1).toString)
      _ <- putStrLn(WFG3(z2, k2, m2).toString)
      _ <- putStrLn("WFG4")
      _ <- putStrLn(WFG4(z1, k1 + 1, m1).toString)
      _ <- putStrLn(WFG4(z2, k2, m2).toString)
      _ <- putStrLn("WFG5")
      _ <- putStrLn(WFG5(z1, k1 + 1, m1).toString)
      _ <- putStrLn(WFG5(z2, k2, m2).toString)
      _ <- putStrLn("WFG6")
      _ <- putStrLn(WFG6(z1, k1 + 1, m1).toString)
      _ <- putStrLn(WFG6(z2, k2, m2).toString)
      _ <- putStrLn("WFG7")
      _ <- putStrLn(WFG7(z1, k1 + 1, m1).toString)
      _ <- putStrLn(WFG7(z2, k2, m2).toString)
      _ <- putStrLn("WFG8")
      _ <- putStrLn(WFG8(z1, k1 + 1, m1).toString)
      _ <- putStrLn(WFG8(z2, k2, m2).toString)
      _ <- putStrLn("WFG9")
      _ <- putStrLn(WFG9(z1, k1 + 1, m1).toString)
      _ <- putStrLn(WFG9(z2, k2, m2).toString)
      _ <- putStrLn("I1")
      _ <- putStrLn(I1(z1, k1 + 1, m1).toString)
      _ <- putStrLn(I1(z2, k2, m2).toString)
      _ <- putStrLn("I2")
      _ <- putStrLn(I2(z1, k1 + 1, m1).toString)
      _ <- putStrLn(I2(z2, k2, m2).toString)
      _ <- putStrLn("I3")
      _ <- putStrLn(I3(z1, k1 + 1, m1).toString)
      _ <- putStrLn(I3(z2, k2, m2).toString)
      _ <- putStrLn("I4")
      _ <- putStrLn(I4(z1, k1 + 1, m1).toString)
      _ <- putStrLn(I4(z2, k2, m2).toString)
      _ <- putStrLn("I5")
      _ <- putStrLn(I5(z1, k1 + 1, m1).toString)
      _ <- putStrLn(I5(z2, k2, m2).toString)
    } yield ""
  }

}
