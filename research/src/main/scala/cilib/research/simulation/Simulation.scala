package cilib.research.simulation

import java.io.File

import cilib.exec.Runner.measureWithInfo
import cilib.exec.{Measurement, Progress, Runner}
import cilib.io.csvSinkAppend
import cilib.research.core.{Archive, Benchmark}
import cilib.research.mgpso.MGParticle._
import cilib.research.mgpso._
import cilib.research.{MGArchive, _}
import cilib.{Iteration, _}
import eu.timepit.refined.auto._
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO.putStrLn
import scalaz.effect._
import scalaz.stream.{Process, merge}

object Simulation {

  def runIO(environments: NonEmptyList[Benchmark],
            strat: (Double, Benchmark) => LambdaStrategy,
            iterations: Int,
            independentRuns: Int,
            stratName: String,
            fileName: String) =
    for {
    _ <- IO { clearFile(fileName) }
    _ <- putStrLn("Executing " + independentRuns + " independent runs.")
    _ <- putStrLn("Each independent run consists of " + environments.size + " problems.")
    _ <- putStrLn("Each problem lasts " + iterations + " iterations.")
    _ <- putStrLn("Saving results to " + fileName + ".")
    timeTaken <- IO {
      val start = System.nanoTime()
      run(environments, strat, iterations, independentRuns, stratName, fileName)
      val finish = System.nanoTime()
      ((finish - start)/ 1000000000).toDouble
    }
    _ <- putStrLn("Complete. Time taken: " + timeTaken + "s")
  } yield ()

  private def run(environments: NonEmptyList[Benchmark],
                  strat: (Double, Benchmark) => LambdaStrategy,
                  iterations: Int,
                  independentRuns: Int,
                  stratName: String,
                  fileName: String) =
    for (x <- 1 to independentRuns) {
      val rng = RNG.init(10L + x.toLong)

      RList.reset(rng, independentRuns)
      val simulations: List[Process[Task, Progress[(MGArchive, NonEmptyList[MGParticle])]]] =
        environments
          .map(env => {
            val swarm = createCollection(strat, env)
            Runner.foldStepS(
              placeholderENV,
              Archive.bounded[MGParticle](50, Dominates(env), CrowdingDistance.mostCrowded),
              rng,
              swarm,
              Runner.staticAlgorithm(stratName, Iteration.syncS(MGPSO.mgpso(env))),
              env.toStaticProblem,
              (x: NonEmptyList[MGParticle], _: Eval[NonEmptyList, Double]) => RVar.pure(x)
            )
          })
          .toList

      simulations.foreach(simulation => {
        val measured: Process[Task, Process[Task, Measurement[String]]] =
          Process.emitAll(List(simulation).map(_.take(iterations).pipe(measurement(x))))

        merge
          .mergeN(20)(measured)
          .to(csvSinkAppend[String](new File(fileName)))
          .run
          .unsafePerformSync
      })
    }

  private def clearFile(fileName: String) = {
    val fileWriter = new java.io.PrintWriter(new File(fileName))
    fileWriter.println("")
  }

  private def measurement(run: Int) =
    measureWithInfo[(MGArchive, NonEmptyList[MGParticle]), Unit, String]((info, collection) =>
      ResultsToJson.finalArchive(run, info.iteration, collection._1)
    )

}
