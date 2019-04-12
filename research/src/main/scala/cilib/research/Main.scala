package cilib
package research

import cilib.research.core.BenchmarkSuite
import cilib.research.mgpso.LambdaStrategy
import cilib.research.simulation.Simulation
import scalaz._
import scalaz.effect.IO._
import scalaz.effect.SafeApp

object Main extends SafeApp {

  // args -> lambda strategy, benchmark suite
  override def run(args: ImmutableArray[String]) = {

    val benchmarkSuite = args(1) match {
      case "ZDT" => BenchmarkSuite.ZDT
      case "WFG.2D" => BenchmarkSuite.WFG_2D
      case "WFG.3D" => BenchmarkSuite.WFG_3D
      case "WFG6.2D" => BenchmarkSuite.OnlyWFG6_2D
    }

    val simulationsIO = benchmarkSuite.benchmarks.traverse1(benchmark => {
      val bounds = benchmark.bounds

      val lambdaStrategy = args(0) match {
        case "STD" => LambdaStrategy.Standard(bounds)
        case "LI" => LambdaStrategy.LinearIncreasing(bounds)
        case "LD" => LambdaStrategy.LinearDecreasing(bounds)
        case "R" => LambdaStrategy.Random(bounds)
        case "RI" => LambdaStrategy.RandomI(bounds)
        case "RIJ" => LambdaStrategy.RandomIJ(bounds)
      }

      Simulation.runIO(lambdaStrategy, benchmark, 2000, 30)
    })

    for {
      _ <- putStrLn("Starting")
      _ <- simulationsIO
      _ <- putStrLn("Done")
    } yield ()

  }

}
