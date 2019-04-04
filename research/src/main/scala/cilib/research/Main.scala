package cilib
package research

import cilib.research.benchmarks.wfg.WFG._
import cilib.research.benchmarks.zdt.ZDT._
import cilib.research.mgpso.LambdaStrategy
import cilib.research.simulation.Simulation
import scalaz._
import scalaz.effect.{IO, SafeApp}

object Main extends SafeApp {

  val envs = NonEmptyList(
    (wfg2objEnvs, "WFG.2D"),
    (wfg3objEnvs, "WFG.3D"),
    (zdtEnvs, "ZDT")
  )

  val ugh = wfg2objEnvs.head

  val strats = NonEmptyList(
    (LambdaStrategy.std _, "STD"),
    (LambdaStrategy.linearIncreasing _, "LI"),
    (LambdaStrategy.linearDecreasing _, "LD"),
    (LambdaStrategy.random _, "R"),
    (LambdaStrategy.random_i _, "RI"),
    (LambdaStrategy.random_i_j _, "RIJ")
  )

  val iterations = 2000
  val runs = 1
  /*
    for (envList <- envs) {
      for (strat <- strats) {
        Simulation.runIO(envList._1, strat._1, itterations, runs, strat._2, strat._2 + "." + envList._2 + ".csv")
      }
    }*/

  override val runc: IO[Unit] = {
    Simulation.runIO(NonEmptyList(ugh),
                     strats.head._1,
                     iterations,
                     runs,
                     strats.head._2,
                     strats.head._2 + "." + envs.head._2)
  }

}
