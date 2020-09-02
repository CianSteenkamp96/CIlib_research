package cilib.research.simulation

import java.io.File

import cilib.exec.Runner.measureWithInfo
import cilib.exec.{Progress, Runner}
import cilib.io.csvSinkAppend
import cilib.research.core.{Archive, Benchmark, GetIndices}
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

// Note w.r.t the RW-PMGPSO:
// Uniform random selection: This implies that each objective has the same probability of being selected, i.e. 1/n_k, where n_k is the number of objectives. So, given a sufficient number of applications of the random selection, the frequency of selection of each objective will be the same for all of the objectives.
// Roulette wheel selection will make sense if you base the probability on same performance information. So, in terms of sub-objectives, if a sub-objective was selected and it resulted in an improvement of performance, then increase the selection probability for that sub-objective, while others will have to reduce 9sum of probabilities has to be equal to 1). So the probability of selection is performance of the sub-objective divided by the sum of performances for the other objectives. This assumes maximization. The issue is now which performance measure to use: Hypervolume? If you do not make use of such a performance-based approach to the Roulette-wheel selection, then it does not make sense.
// If what you have done is not similar to what I have mentioned under point 2, then my suggestion is rather to stick to the standard approach. Otherwise you will have to spend more time on developing an appropriate performance-based probability approach.
// i.e. the roulette wheel partial-dominance implementation (the RW-PMGPSO algorithm) needs some work to be a viable option.

object Simulation {

  def runIO(algoName: String,
            numObjectives: Int,
            numDecisionVariables: Int,
            lambdaStrategy: LambdaStrategy,
            benchmark: Benchmark,
            iterations: Int,
            independentRuns: Int,
            // For KnMGPSO only
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
          Archive.boundedPD[MGParticle](
            popSize,
            PartiallyDominates(benchmark),
            CrowdingDistance.mostCrowded,
            numObjectives) // to know in which range to randomly choose objectives
        else if (algoName == "RW-PMGPSO")
          Archive.boundedRWPD[MGParticle](
            popSize,
            PartiallyDominates(benchmark),
            CrowdingDistance.mostCrowded,
            List.fill(numObjectives)(0).toNel.get,
            GetIndices.get3IndicesPD(numObjectives)) // initially choose 3 objectives randomly
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
            // to avoid type errors ...
            getFitness,
            toMGParticleNel,
            take2
          )
        } else
          throw new Exception(
            "The algorithm name should be \"MGPSO\", \"PMGPSO\", \"RW-PMGPSO\", or \"KnMGPSO\".")

      val simulation: Process[Task, Progress[(MGArchive, NonEmptyList[MGParticle])]] = {
        Runner.foldStepS(
          placeholderENV,
          archive,
          rng,
          swarm,
          Runner.staticAlgorithm(lambdaStrategy.name, Iteration.syncS(MGPSO.pso(benchmark))),
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
