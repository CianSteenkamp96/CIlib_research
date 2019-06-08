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
import eu.timepit.refined.api.Refined /////////////////////////// NEW ///////////////////////////
import eu.timepit.refined.refineV /////////////////////////// NEW ///////////////////////////
import eu.timepit.refined.numeric.Positive /////////////////////////// NEW ///////////////////////////
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO._
import scalaz.effect._
import scalaz.stream.{Process, merge}

object Simulation {

  def runIO(algoName: String, /////////////////////////// New //////////////////////////////////
            numObjectives: Int, /////////////////////////// New //////////////////////////////////
            numDecisionVariables: Int, /////////////////////////// New //////////////////////////////////
            lambdaStrategy: LambdaStrategy,
            benchmark: Benchmark,
            iterations: Int,
            independentRuns: Int): IO[Unit] =
    for {
      _ <- IO(clearFile(algoName + "." + lambdaStrategy.name + "." + benchmark.name + "." + numObjectives + "obj." + numDecisionVariables + "D"))
      _ <- (1 to independentRuns).toList.traverse(runCount => {

        val rng = RNG.init(10L + runCount.toLong)
        val swarm = createCollection(benchmark, lambdaStrategy.evalValue(rng))

        /////////////////////////////////////////////// NEW //////////////////////////////////////////////////
        val popSize: Int Refined Positive = refineV[Positive](benchmark.controlParameters.swarmSizes.foldLeft(0)(_ + _)).right.get // archive limit set equal to total population size by default
        /////////////////////////////////////////////// NEW //////////////////////////////////////////////////
        val archive = if (algoName == "MGPSO") Archive.bounded[MGParticle](popSize, Dominates(benchmark), CrowdingDistance.mostCrowded)
        else if(algoName == "PMGPSO") Archive.boundedPD[MGParticle](popSize, PartiallyDominates(benchmark), CrowdingDistance.mostCrowded, List.fill(numObjectives)(0).toNel.get, (0, 1, 2))
        else throw new Exception("The algorithm name should be \"MGPSO\" or \"PMGPSO\".")

        val simulation: Process[Task, Progress[(MGArchive, NonEmptyList[MGParticle])]] = {
          Runner.foldStepS(
            placeholderENV,
            archive,
            rng,
            swarm,
            Runner.staticAlgorithm(lambdaStrategy.name, Iteration.syncS(MGPSO.mgpso(benchmark))),
            benchmark.toStaticProblem,
            (x: NonEmptyList[MGParticle], _: Eval[NonEmptyList, Double]) => RVar.pure(x)
          )
        }

        val measured: Process[Task, Process[Task, Measurement[String]]] =
          Process.emitAll(List(simulation).map(_.take(iterations).pipe(measurement(runCount, iterations))))

        val stream = merge
          .mergeN(20)(measured)
          .to(csvSinkAppend[String](new File(algoName + "." + lambdaStrategy.name + "." + benchmark.name + "." + numObjectives + "obj." + numDecisionVariables + "D")))
          .run

        for {
          _ <- putStr(List(algoName, lambdaStrategy.name, benchmark.name, numObjectives + "obj", numDecisionVariables + "D", "run: " + runCount).mkString(" - "))
          timeTaken <- IO {
            val start = System.nanoTime()
            stream.unsafePerformSync
            val finish = System.nanoTime()
            ((finish - start) / 1000000000).toDouble
          }
          _ <- putStr(" -- Time taken: " + timeTaken + "s")
          _ <- putStrLn("")
        } yield ()
      })
    } yield ()

  private def measurement(run: Int, maxIterations: Int) =
    measureWithInfo[(MGArchive, NonEmptyList[MGParticle]), Unit, String]((info, collection) =>
      ResultsToJson.finalArchive(run, info.iteration, collection._1, maxIterations))

  private def clearFile(fileName: String): Unit = {
    val fileWriter = new java.io.PrintWriter(new File(fileName))
    fileWriter.println("")
  }

}
