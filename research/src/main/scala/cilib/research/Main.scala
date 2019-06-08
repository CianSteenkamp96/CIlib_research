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

  // args -> lambda strategy, benchmark suite
  override def run(args: ImmutableArray[String]): IO[Unit] = {

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CONFIG CHANGES HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    val benchmarkSuite =
    //      BenchmarkSuite.wfg3obj
    //      BenchmarkSuite.wfg5obj
    //      BenchmarkSuite.wfg8obj
    //      BenchmarkSuite.wfg10obj
    //      BenchmarkSuite.wfg15obj

    //      BenchmarkSuite.dtlz3obj
    //      BenchmarkSuite.dtlz5obj
    //      BenchmarkSuite.dtlz8obj
    //      BenchmarkSuite.dtlz10obj
      BenchmarkSuite.dtlz15obj

    val simulationsIO = benchmarkSuite.benchmarks.traverse1(benchmark => {
      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CONFIG CHANGES HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      val bounds = benchmark.bounds
      val lambdaStrategy =
//              LambdaStrategy.Standard(bounds)
      //        LambdaStrategy.LinearIncreasing(bounds)
      //        LambdaStrategy.LinearDecreasing(bounds)
              LambdaStrategy.Random(bounds)
      //        LambdaStrategy.RandomI(bounds)
      //        LambdaStrategy.RandomIJ(bounds)

      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CONFIG CHANGES HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      // Algo name => MGPSO or PMGPSO
      Simulation.runIO("PMGPSO", 15, lambdaStrategy, benchmark, 100, 3)
    })

    for {
      _ <- putStrLn("Starting")
      _ <- simulationsIO
      _ <- putStrLn("Done")
    } yield ()
  }
}
