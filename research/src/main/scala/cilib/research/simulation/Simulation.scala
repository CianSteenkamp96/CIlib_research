package cilib.research.simulation

import java.io.File

import cilib.exec.Runner.measureWithInfo
import cilib.exec.{Progress, Runner}
import cilib.io.csvSinkAppend
import cilib.research.core.{Archive, Benchmark}
import cilib.research.mgpso.MGParticle._
import cilib.research.mgpso._
import cilib.research.{MGArchive, _}
import cilib.{Iteration, _}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO._
import scalaz.effect._
import scalaz.stream.{Process, merge}

object Simulation {

  def runIO(algoName: String,
            numObjectives: Int,
            numDecisionVariables: Int,
            lambdaStrategy: LambdaStrategy,
            benchmark: Benchmark,
            iterations: Int,
            independentRuns: Int,
            desired_ratio_KPs_2_ND_sols: Double = -1.0): IO[Unit] = {
    val measuredSimulations = (1 to independentRuns).map(runCount => {
      val rng = RNG.init(10L + runCount.toLong)
      val swarm = createCollection(benchmark, lambdaStrategy.evalValue(rng))
      val popSize: Int Refined Positive = refineV[Positive](
        benchmark.controlParameters.swarmSizes
          .foldLeft(0)(_ + _)).right.get // archive limit set equal to total population size by default
      val archive =
        if (algoName == "MGPSO" || algoName == "KnMGPSO")
          Archive.bounded[MGParticle](popSize, Dominates(benchmark), CrowdingDistance.mostCrowded)
        else if (algoName == "PMGPSO")
          Archive.boundedPD[MGParticle](popSize,
                                        PartiallyDominates(benchmark),
                                        CrowdingDistance.mostCrowded,
                                        List.fill(numObjectives)(0).toNel.get,
                                        (0, 1, 2))
        else
          throw new Exception("The algorithm name should be \"MGPSO\", \"PMGPSO\", or \"KnMGPSO\".")

      if (algoName == "KnMGPSO" && desired_ratio_KPs_2_ND_sols > 0 && desired_ratio_KPs_2_ND_sols < 1) {
        val simulation: Process[Task, Progress[(MGArchive, NonEmptyList[MGParticle])]] = {
          Runner.foldStepS(
            placeholderENV,
            archive,
            rng,
            swarm,
            Runner.staticAlgorithm(
              lambdaStrategy.name,
              Iteration.syncS(MGPSO.mgpso_pmgpso_knmgpso(benchmark, desired_ratio_KPs_2_ND_sols))),
            benchmark.toStaticProblem,
            (x: NonEmptyList[MGParticle], _: Eval[NonEmptyList, Double]) => RVar.pure(x)
          )
        }
        simulation.take(iterations).pipe(measurement(runCount, iterations))
      } else {
        // if desired_ratio_KPs_2_ND_sols is not equal to -1 at this point it WILL CAUSE KAK because checks are done throughout (MGPSO.scala and KneePoint.scala)
        // to check if desired_ratio_KPs_2_ND_sols is not equal to -1, in which case it is assumed that we are deailing with the KnMGPSO. Note the best way but heyyyy :)
        assert(desired_ratio_KPs_2_ND_sols == -1.0)
        val simulation: Process[Task, Progress[(MGArchive, NonEmptyList[MGParticle])]] = {
          Runner.foldStepS(
            placeholderENV,
            archive,
            rng,
            swarm,
            Runner.staticAlgorithm(lambdaStrategy.name,
                                   Iteration.syncS(MGPSO.mgpso_pmgpso_knmgpso(benchmark))),
            benchmark.toStaticProblem,
            (x: NonEmptyList[MGParticle], _: Eval[NonEmptyList, Double]) => RVar.pure(x)
          )
        }
        simulation.take(iterations).pipe(measurement(runCount, iterations))
      }
    })

    val stream = merge
      .mergeN(24)(Process.emitAll(measuredSimulations))
      .to(csvSinkAppend[String](new File(
        algoName + "." + lambdaStrategy.name + "." + benchmark.name + "." + numObjectives + "obj")))

    for {
      _ <- IO(clearFile(
        algoName + "." + lambdaStrategy.name + "." + benchmark.name + "." + numObjectives + "obj"))
      timeTaken <- IO {
        val start = System.nanoTime()
        stream.run.unsafePerformSync
        val finish = System.nanoTime()
        ((finish - start) / 1000000000).toDouble
      }
      _ <- putStr(
        algoName + "." + lambdaStrategy.name + "." + benchmark.name + "." + numObjectives + "obj." + numDecisionVariables + "D => " + independentRuns + " independent runs with " + iterations + " iterations each -- Time taken: " + timeTaken + "s")
      _ <- putStrLn("")
    } yield ()
  }

  private def measurement(run: Int, maxIterations: Int) =
    measureWithInfo[(MGArchive, NonEmptyList[MGParticle]), Unit, String]((info, collection) =>
      ResultsToJson.finalArchive(run, info.iteration, collection._1, maxIterations))

  private def clearFile(fileName: String): Unit = {
    val fileWriter = new java.io.PrintWriter(new File(fileName))
    fileWriter.println("")
  }

}
