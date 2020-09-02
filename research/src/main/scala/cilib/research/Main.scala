package cilib
package research

import cilib.research.core.BenchmarkSuite
import cilib.research.mgpso.LambdaStrategy
import cilib.research.simulation.Simulation
import scalaz._
import scalaz.effect.IO._
import scalaz.effect.{IO, SafeApp}
import spire.math.Interval

// Note w.r.t the RW-PMGPSO:
// Uniform random selection: This implies that each objective has the same probability of being selected, i.e. 1/n_k, where n_k is the number of objectives. So, given a sufficient number of applications of the random selection, the frequency of selection of each objective will be the same for all of the objectives.
// Roulette wheel selection will make sense if you base the probability on same performance information. So, in terms of sub-objectives, if a sub-objective was selected and it resulted in an improvement of performance, then increase the selection probability for that sub-objective, while others will have to reduce 9sum of probabilities has to be equal to 1). So the probability of selection is performance of the sub-objective divided by the sum of performances for the other objectives. This assumes maximization. The issue is now which performance measure to use: Hypervolume? If you do not make use of such a performance-based approach to the Roulette-wheel selection, then it does not make sense.
// If what you have done is not similar to what I have mentioned under point 2, then my suggestion is rather to stick to the standard approach. Otherwise you will have to spend more time on developing an appropriate performance-based probability approach.
// i.e. the roulette wheel partial-dominance implementation (the RW-PMGPSO algorithm) needs some work to be a viable option.

// To create jars see phase1/readmes/jar_creation on MSc HD (sbt clean packArchive)

// To run -> click on Main (next to green hammer) -> edit configs -> add command line args to 'Program arguments:' -> apply -> run

// command line args example 1 => MGPSO STD 3 30 WFG1
// command line args example 2 => KnMGPSO R 15 30 WFG 0.5
// command line args example 3 => KnMGPSO RI 10 30 DTLZ7 0.2
object Main extends SafeApp {

  override def run(args: ImmutableArray[String]): IO[Unit] = {

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! HARDCODED CONFIG CHANGES HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val iterations: Int = 2000 // #iterations per independent sample/run !!! NB HERE !!!
    val independentRuns: Int = 30 // #independent samples !!! NB HERE !!!
    val algos: List[String] = List("MGPSO", "PMGPSO", "RW-PMGPSO", "KnMGPSO")
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // These checks are specific for my research
    // asserts vs throwing exceptions w.r.t functional programming? Not purely functional - but good enough for my purposes here.
    // Throwing exceptions not pure - maybe improve at a later stage
    if (args.length != 5 && args.length != 6)
      throw new Exception(
        "\nNumber of CL params needs to be at least 5 separated by spaces including: algoName (MGPSO, PMGPSO, RW-PMGPSO, or KnMGPSO) archiveCoeffUpdateStrategy (STD, R, RI, or RIJ) numObjs (3, 5, 8, 10, or 15) numDecisionVariables (30, 100, 500, or 1000) problemSuite (WFG or DTLZ) | problemSuiteAndproblemNum (WFG1, ..., or WFG9 or DTLZ1, ..., or DTLZ7) desiredRatioKP2ND (*** For KnMGPSO only ***; any value in range (0, 1)).\n")

    // simulation specs passed via command line args
    val algoName: String = args(0)

    if (!algos.contains(algoName))
      throw new Exception(
        "\nAlgorithm (names) allowed: \"MGPSO\", \"PMGPSO\", \"RW-PMGPSO\", \"KnMGPSO\".\n")

    // this stops the RW-PMGPSO for now... needs some work... see top of file.
    if (algoName == "RW-PMGPSO")
      throw new Exception(
        "\nNote w.r.t the RW-PMGPSO:\nUniform random selection: This implies that each objective has the same probability of being selected, i.e. 1/n_k, where n_k is the number of objectives. So, given a sufficient number of applications of the random selection, the frequency of selection of each objective will be the same for all of the objectives.\nRoulette wheel selection will make sense if you base the probability on same performance information. So, in terms of sub-objectives, if a sub-objective was selected and it resulted in an improvement of performance, then increase the selection probability for that sub-objective, while others will have to reduce 9sum of probabilities has to be equal to 1). So the probability of selection is performance of the sub-objective divided by the sum of performances for the other objectives. This assumes maximization. The issue is now which performance measure to use: Hypervolume? If you do not make use of such a performance-based approach to the Roulette-wheel selection, then it does not make sense.\nIf what you have done is not similar to what I have mentioned under point 2, then my suggestion is rather to stick to the standard approach. Otherwise you will have to spend more time on developing an appropriate performance-based probability approach.\ni.e. the roulette wheel partial-dominance implementation (the RW-PMGPSO algorithm) needs some work to be a viable option.\n")

    if (algoName == "KnMGPSO" && args.size != 6)
      throw new Exception(
        "\nCL params needs to be: algoName (MGPSO, PMGPSO, RW-PMGPSO, or KnMGPSO) archiveCoeffUpdateStrategy (STD, R, RI, or RIJ) numObjs (3, 5, 8, 10, or 15) numDecisionVariables (30, 100, 500, or 1000) problemSuite (WFG or DTLZ) | problemSuiteAndproblemNum (WFG1, ..., or WFG9 or DTLZ1, ..., or DTLZ7) desiredRatioKP2ND (*** For KnMGPSO only ***; any value in range [0, 1]).\n")

    val archiveCoeffUpdateStrategy: String = args(1)
    val numObj: Int = args(2).toInt
    val numDecisionVariables: Int = args(3).toInt
    val problem: String = args(4)
    val desiredRatioKP2ND: Double = if (args.size == 6) args(5).toDouble else -1.0

    if (algoName != "KnMGPSO" && desiredRatioKP2ND != -1.0)
      throw new Exception("\ndesiredRatioKP2ND only relevant for KnMGPSO.\n")
    else if (algoName == "KnMGPSO" && desiredRatioKP2ND <= 0 || desiredRatioKP2ND >= 1)
      throw new Exception("\ndesiredRatioKP2ND should be specified in range (0, 1).\n")

    if ((algoName != "MGPSO" && algoName != "PMGPSO" && algoName != "RW-PMGPSO" && algoName != "KnMGPSO") ||
        (archiveCoeffUpdateStrategy != "STD" && archiveCoeffUpdateStrategy != "R" && archiveCoeffUpdateStrategy != "RI" && archiveCoeffUpdateStrategy != "RIJ") ||
        (numObj != 3 && numObj != 5 && numObj != 8 && numObj != 10 && numObj != 15) ||
        (numDecisionVariables != 30 && numDecisionVariables != 100 && numDecisionVariables != 500 && numDecisionVariables != 1000) ||
        (!problem.contains("WFG") && !problem.contains("DTLZ")))
      throw new Exception(
        "\nCL params needs to be: algoName (MGPSO, PMGPSO, RW-PMGPSO, or KnMGPSO) archiveCoeffUpdateStrategy (STD, R, RI, or RIJ) numObjs (3, 5, 8, 10, or 15) numDecisionVariables (30, 100, 500, or 1000) problemSuite (WFG or DTLZ) | problemSuiteAndproblemNum (WFG1, ..., or WFG9 or DTLZ1, ..., or DTLZ7) desiredRatioKP2ND (*** For KnMGPSO only ***; any value in range [0, 1]).\n")

    if (problem.contains("WFG")) {
      if (problem.length > 4)
        throw new Exception("\nThe problem number for WFG should be: 1, 2, ..., or 9.\n") // DTLZ or DTLZ1 (DTLZ or DTLZ<problem number>)
      if (problem.length == 4)
        if (problem.reverse.head.toString.toInt != 1 && problem.reverse.head.toString.toInt != 2 && problem.reverse.head.toString.toInt != 3 && problem.reverse.head.toString.toInt != 4 && problem.reverse.head.toString.toInt != 5 && problem.reverse.head.toString.toInt != 6 && problem.reverse.head.toString.toInt != 7 && problem.reverse.head.toString.toInt != 8 && problem.reverse.head.toString.toInt != 9)
          throw new Exception("\nThe problem number for WFG should be: 1, 2, ..., or 9.\n")
    } else if (problem.contains("DTLZ")) {
      if (problem.length > 5)
        throw new Exception("\nThe problem number for DTLZ should be: 1, 2, ..., or 7.\n") // DTLZ or DTLZ1 (DTLZ or DTLZ<problem number>)
      if (problem.length == 5)
        if (problem.reverse.head.toString.toInt != 1 && problem.reverse.head.toString.toInt != 2 && problem.reverse.head.toString.toInt != 3 && problem.reverse.head.toString.toInt != 4 && problem.reverse.head.toString.toInt != 5 && problem.reverse.head.toString.toInt != 6 && problem.reverse.head.toString.toInt != 7)
          throw new Exception("\nThe problem number for DTLZ should be: 1, 2, ..., or 7.\n")
    } else
      throw new Exception("\nTest suites supported: \"WFG\" or \"DTLZ\".\n")

    val benchmarkSuiteName: String =
      if (problem.contains("WFG")) "WFG"
      else if (problem.contains("DTLZ")) "DTLZ"
      else throw new Exception("\nTest suites supported: \"WFG\" or \"DTLZ\".\n") // problem suite: 'WFG' or 'DTLZ'

    val problemNum: Int =
      if (problem.contains("WFG") && problem.length == 4 || problem
            .contains("DTLZ") && problem.length == 5) problem.reverse.head.toString.toInt
      else -1

    val subswarms3obj: NonEmptyList[Int] = NonEmptyList(51, 51, 51) // 153
    val subswarms5obj: NonEmptyList[Int] = NonEmptyList(25, 25, 25, 25, 26) // 126
    val subswarms8obj: NonEmptyList[Int] = NonEmptyList(19, 19, 19, 19, 19, 19, 19, 20) // 156
    val subswarms10obj
      : NonEmptyList[Int] = NonEmptyList(11, 11, 11, 11, 11, 11, 11, 11, 11, 11) // 110
    val subswarms15obj: NonEmptyList[Int] =
      NonEmptyList(9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9) // 135

    // NonEmptyList of sub swarm divisions;
    // number of objectives equal to the size of the swarms NonEmptyList,
    // that is, each element in the NEL represents a sub swarm's size
    val swarms: NonEmptyList[Int] =
      if (numObj == 3) subswarms3obj
      else if (numObj == 5) subswarms5obj
      else if (numObj == 8) subswarms8obj
      else if (numObj == 10) subswarms10obj
      else if (numObj == 15) subswarms15obj
      else throw new Exception("\nNumber of objectives supported: 3, 5, 8, 10 or 15.\n")

    assert(numObj == swarms.size)

    val benchmarkSuite: BenchmarkSuite =
      if (benchmarkSuiteName == "WFG")
        BenchmarkSuite.wfgObj(numObj, numDecisionVariables, swarms, problemNum)
      else if (benchmarkSuiteName == "DTLZ")
        BenchmarkSuite.dtlzObj(numObj, numDecisionVariables, swarms, problemNum)
      else
        throw new Exception("\nTest suites supported: \"WFG\" or \"DTLZ\".\n")

    val simulationsIO: IO[NonEmptyList[Unit]] = benchmarkSuite.benchmarks.traverse1(b => {
      val bounds: NonEmptyList[Interval[Double]] = b.bounds
      val ls: LambdaStrategy =
        if (archiveCoeffUpdateStrategy == "STD")
          LambdaStrategy.Standard(bounds)
        else if (archiveCoeffUpdateStrategy == "R")
          LambdaStrategy.Random(bounds)
        else if (archiveCoeffUpdateStrategy == "RI")
          LambdaStrategy.RandomI(bounds)
        else
          throw new Exception(
            "\nArchive balance coefficient update strategies supported: \"STD\", \"R\", or \"RI\".\n")

      Simulation.runIO(algoName,
                       numObj,
                       numDecisionVariables,
                       ls,
                       b,
                       iterations,
                       independentRuns,
                       desiredRatioKP2ND)
    })

    for {
      _ <- putStrLn("Starting")
      _ <- simulationsIO
      _ <- putStrLn("Done")
    } yield ()
  }
}
