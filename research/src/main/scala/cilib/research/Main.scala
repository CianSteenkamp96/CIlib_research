package cilib
package research

import cilib.research.core.BenchmarkSuite
import cilib.research.mgpso.LambdaStrategy
import cilib.research.simulation.Simulation
import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO._
import scalaz.effect.{IO, SafeApp}

object Main extends SafeApp {

  override def run(args: ImmutableArray[String]): IO[Unit] = {
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CONFIG CHANGES HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // This will be passed in through the CLI
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    val algoName
      : String = "PMGPSO" // 'MGPSO' or 'PMGPSO' - still to possibly implement KnMGPSO_A, KnMGPSO_B, KnPMGPSO_A, KnPMGPSO_B -
    // see note in office cupboard explaining different Knea Point MGPSO options/configs/combos of insert policies and archive guide selection
    val lambdaStrategy: String = "R" // 'R', 'STD', 'LI', 'LD', 'RI', or 'RIJ'
    val iterations: Int = 100 // #iterations per independent sample/run
    val independentRuns: Int = 3 // #independent samples
    val numDecisionVariables: Int = 30 // #dimensions in the decision space
    val swarms
      : NonEmptyList[Int] = List.fill(15)(10).toNel.get // NonEmptyList of sub swarm divisions;
    // number of objectives equal to the size of the swarms NonEmptyList,
    // that is, each element in the NEL represents a sub swarm's size
    val benchmarkSuiteName: String = "WFG" // 'WFG','DTLZ', or 'ZDT'
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    val numObjectives: Int = swarms.size // #dimensions in the objective/solution space

    val benchmarkSuite =
      if (benchmarkSuiteName == "WFG")
        BenchmarkSuite.wfgObj(numObjectives, numDecisionVariables, swarms)
      else if (benchmarkSuiteName == "DTLZ")
        BenchmarkSuite.dtlzObj(numObjectives, numDecisionVariables, swarms)
      else if (benchmarkSuiteName == "ZDT")
        BenchmarkSuite.zdtObj(numObjectives = 2, swarms = swarms) // ZDT is only bi-objective and has its own specific number of decision variables
      else
        throw new Exception(
          "Test suite should be one of the following: \"WFG\", \"DTLZ\", or \"ZDT\"")

    val simulationsIO = benchmarkSuite.benchmarks.traverse1(b => {
      val bounds = b.bounds
      val ls =
        if (lambdaStrategy == "STD")
          LambdaStrategy.Standard(bounds)
        else if (lambdaStrategy == "LI")
          LambdaStrategy.LinearIncreasing(bounds)
        else if (lambdaStrategy == "LD")
          LambdaStrategy.LinearDecreasing(bounds)
        else if (lambdaStrategy == "R")
          LambdaStrategy.Random(bounds)
        else if (lambdaStrategy == "RI")
          LambdaStrategy.RandomI(bounds)
        else if (lambdaStrategy == "RIJ")
          LambdaStrategy.RandomIJ(bounds)
        else
          throw new Exception(
            "Lambda strategy can only be one of the following: \"STD\", \"LI\", \"LD\", \"R\", \"RI\", or \"RIJ\"")
      Simulation.runIO(algoName,
                       numObjectives,
                       numDecisionVariables,
                       ls,
                       b,
                       iterations,
                       independentRuns)
    })

    for {
      _ <- putStrLn("Starting")
      _ <- simulationsIO
      _ <- putStrLn("Done")
    } yield ()
  }
}
