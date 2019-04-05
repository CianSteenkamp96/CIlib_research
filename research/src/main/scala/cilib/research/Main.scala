package cilib
package research

import cilib.research.core.BenchmarkSuite
import cilib.research.mgpso.LambdaStrategy
import cilib.research.simulation.SimulationConfig
import scalaz._
import scalaz.effect.IO._
import scalaz.effect.{IO, SafeApp}

object Main extends SafeApp {

  // args -> lambda strategy, benchmark suite
  override def run(args: ImmutableArray[String]) = {

    val iterations = 2000
    val runs = 1

    val benchmarkSuite = args(1) match {
    case "ZDT"    => BenchmarkSuite.ZDT
    case "WFG.2D" => BenchmarkSuite.WFG_2D
    case "WFG.3D" => BenchmarkSuite.WFG_3D
  }

    val configs = benchmarkSuite.benchmarks.map(benchmark => {
      val bounds = benchmark.bounds

      val lambdaStrategy = args(0) match {
        case "STD" => LambdaStrategy.Standard(bounds)
        case "LI"  => LambdaStrategy.Standard(bounds)
        case "LD"  => LambdaStrategy.Standard(bounds)
        case "R"   => LambdaStrategy.Standard(bounds)
        case "RI"  => LambdaStrategy.Standard(bounds)
        case "RIJ" => LambdaStrategy.Standard(bounds)
      }

      SimulationConfig(benchmark, lambdaStrategy)
    })

    val a = configs.traverse1(x => IO(x))



  }

}
