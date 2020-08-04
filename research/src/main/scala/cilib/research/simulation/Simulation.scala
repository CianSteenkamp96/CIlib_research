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
            // desired_ratio_KPs_2_ND_sols => user defined parameter > 0 and < 1; represents the desired ratio of knee points to non-dominated solutions
            desired_ratio_KPs_2_ND_sols: Double = -1.0): IO[Unit] = {
    val measuredSimulations = (1 to independentRuns).map(runCount => {
      val rng = RNG.init(10L + runCount.toLong)
      val swarm = createCollection(benchmark, lambdaStrategy.evalValue(rng))
      val popSize: Int Refined Positive = refineV[Positive](
        benchmark.controlParameters.swarmSizes
          .foldLeft(0)(_ + _)).right.get // archive limit set equal to total population size by default
      val archive =
        if (algoName == "MGPSO")
          Archive.bounded[MGParticle](popSize, Dominates(benchmark), CrowdingDistance.mostCrowded)
        else if (algoName == "PMGPSO")
          Archive.boundedPD[MGParticle](popSize,
                                        PartiallyDominates(benchmark),
                                        CrowdingDistance.mostCrowded,
                                        List.fill(numObjectives)(0).toNel.get,
                                        (0, 1, 2))
        else if (algoName == "KnMGPSO") {
          assert(desired_ratio_KPs_2_ND_sols > 0 && desired_ratio_KPs_2_ND_sols < 1)
          // functions to avoid type errors ...
          def getFitness(l: List[MGParticle]): List[List[Double]] = l.map(x => x.pos.fitness.toList)
          def toMGParticleNel(l: List[MGParticle]): NonEmptyList[MGParticle] = l.toNel.get
          def take2(l: List[MGParticle]): List[MGParticle] = l.take(2)
          Archive.boundedKP[MGParticle](
            popSize,
            Dominates(benchmark),
            CrowdingDistance.mostCrowded,
            desired_ratio_KPs_2_ND_sols,
            (1.0, 0.0),
            // this initial NEL does not matter
            NonEmptyList(1.0, 1.0, 1.0),
            getFitness,
            toMGParticleNel,
            take2
          )
        } else
          throw new Exception("The algorithm name should be \"MGPSO\", \"PMGPSO\", or \"KnMGPSO\".")

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
