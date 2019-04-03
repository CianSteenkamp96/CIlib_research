package cilib
package research

import cilib.zdt.ZDT._
import cilib.wfg.WFG._
import scalaz._

object MGPSO {

  def main(args: Array[String]): Unit = {

    val envs = NonEmptyList(
      (wfg2objEnvs, "WFG.2D"),
      (wfg3objEnvs, "WFG.3D"),
      (zdtEnvs, "ZDT")
    )

    val strats = NonEmptyList(
      (Lambda.std _, "STD"),
      (Lambda.linearIncreasing _, "LI"),
      (Lambda.linearDecreasing _, "LD"),
      (Lambda.random _, "R"),
      (Lambda.random_i _, "RI"),
      (Lambda.random_i_j _, "RIJ")
    )

    val itterations = 2
    val runs = 2

    for (envList <- envs) {
      for (strat <- strats) {
        Simulation.runIO(envList._1, strat._1, itterations, runs, strat._2, strat._2 + "." + envList._2 + ".csv")
      }
    }

  }

}
