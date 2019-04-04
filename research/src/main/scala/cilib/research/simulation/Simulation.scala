package cilib.research.simulation

import java.io.File

import cilib.exec.Runner.measure
import cilib.exec.{Measurement, Progress, Runner}
import cilib.io.csvSinkAppend
import cilib.research.core.{Archive, EnvironmentX}
import cilib.research.mgpso.MGParticle._
import cilib.research.mgpso._
import cilib.research.{MGArchive, _}
import cilib.{Iteration, _}
import eu.timepit.refined.auto._
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream.{Process, merge}

final case class Results(run: Int, archive: String)

object Simulation {

  def runIO(environments: NonEmptyList[EnvironmentX],
            strat: (Double, EnvironmentX) => mgpso.Lambda,
            iterations: Int,
    independentRuns: Int,
    stratName: String,
            fileName: String) = {
    clearFile(fileName)
    println("Executing " + independentRuns + " independent runs.")
    println("Each independent run consists of " + environments.size + " problems.")
    println("Each problem lasts " + iterations + " iterations.")
    println("Saving results to " + fileName + ".")
    val t0 = System.nanoTime()
    run(environments, strat, iterations, independentRuns, stratName, fileName)
    val t1 = System.nanoTime()
    println("Complete. Time taken: " + ((t1 - t0) / 1000000000).toDouble + "s")
  }

  def run(environments: NonEmptyList[EnvironmentX],
          strat: (Double, EnvironmentX) => mgpso.Lambda,
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
              Archive.bounded[MGParticle](50, Dominates.apply(env), CrowdingDistance.mostCrowded),
              rng,
              swarm,
              Runner.staticAlgorithm(stratName, Iteration.syncS(MGPSO.mgpso(env))),
              env.toStaticProblem,
              (x: NonEmptyList[MGParticle], _: Eval[NonEmptyList, Double]) => RVar.pure(x)
            )
          })
          .toList

      simulations.foreach(simulation => {
        val measured: Process[Task, Process[Task, Measurement[Results]]] =
          Process.emitAll(List(simulation).map(_.take(iterations).pipe(measurement(x))))

        merge
          .mergeN(20)(measured)
          .to(csvSinkAppend[Results](new File(fileName)))
          .run
          .unsafePerformSync
      })
    }

  def clearFile(fileName: String) = {
    val fileWriter = new java.io.PrintWriter(new File(fileName))
    fileWriter.println("")
  }

  def measurement(run: Int) =
    measure[(MGArchive, NonEmptyList[MGParticle]), Unit, Results](collection =>
      Results(run, collection._1.values.map(x => x.pos.fitness).mkString(",") + "\n"))

}
