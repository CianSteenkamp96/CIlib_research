package cilib.research.core
import cilib.research.benchmarks.wfg._
import cilib.research.benchmarks.dtlz._
import cilib.research.benchmarks.zdt._
import scalaz.NonEmptyList
import spire.math.Interval
import spire.implicits._
import cilib._ ////////////////////////////////////////////// NEW //////////////////////////////////////////////

import scalaz._
import Scalaz._

case class BenchmarkSuite(name: String, benchmarks: NonEmptyList[Benchmark])

////////////////////////////////////////////// NEW //////////////////////////////////////////////
object BenchmarkSuite {

  def wfgObj(numObjectives: Int,
             numDecisionVariables: Int,
             swarms: NonEmptyList[Int]): BenchmarkSuite = {
    val wfgBounds =
      (0 until numDecisionVariables).toList.toNel.get.map(x => Interval(0.0, 2.0 * x + 1.0))
    BenchmarkSuite(
      "wfg" + numObjectives + "obj",
      NonEmptyList(
        Benchmark("WFG1", WFG.WFG1(numObjectives), wfgBounds, ControlParameters(swarms)),
        Benchmark("WFG2", WFG.WFG2(numObjectives), wfgBounds, ControlParameters(swarms)),
        Benchmark("WFG3", WFG.WFG3(numObjectives), wfgBounds, ControlParameters(swarms)),
        Benchmark("WFG4", WFG.WFG4(numObjectives), wfgBounds, ControlParameters(swarms)),
        Benchmark("WFG5", WFG.WFG5(numObjectives), wfgBounds, ControlParameters(swarms)),
        Benchmark("WFG6", WFG.WFG6(numObjectives), wfgBounds, ControlParameters(swarms)),
        Benchmark("WFG7", WFG.WFG7(numObjectives), wfgBounds, ControlParameters(swarms)),
        Benchmark("WFG8", WFG.WFG8(numObjectives), wfgBounds, ControlParameters(swarms)),
        Benchmark("WFG9", WFG.WFG9(numObjectives), wfgBounds, ControlParameters(swarms))
      )
    )
  }

  def dtlzObj(numObjectives: Int,
              numDecisionVariables: Int,
              swarms: NonEmptyList[Int]): BenchmarkSuite = {
    val dtlzBounds =
      (0 until numDecisionVariables).toList.toNel.get.map(x => Interval(0.0, 1.0))
    BenchmarkSuite(
      "dtlz" + numObjectives + "obj",
      NonEmptyList(
        Benchmark("DTLZ1",
                  DTLZ.DTLZ1F(numObjectives, numDecisionVariables),
                  dtlzBounds,
                  ControlParameters(swarms)),
        Benchmark("DTLZ2",
                  DTLZ.DTLZ2F(numObjectives, numDecisionVariables),
                  dtlzBounds,
                  ControlParameters(swarms)),
        Benchmark("DTLZ3",
                  DTLZ.DTLZ3F(numObjectives, numDecisionVariables),
                  dtlzBounds,
                  ControlParameters(swarms)),
        Benchmark("DTLZ4",
                  DTLZ.DTLZ4F(numObjectives, numDecisionVariables),
                  dtlzBounds,
                  ControlParameters(swarms)),
        Benchmark("DTLZ5",
                  DTLZ.DTLZ5F(numObjectives, numDecisionVariables),
                  dtlzBounds,
                  ControlParameters(swarms)),
        Benchmark("DTLZ6",
                  DTLZ.DTLZ6F(numObjectives, numDecisionVariables),
                  dtlzBounds,
                  ControlParameters(swarms)),
        Benchmark("DTLZ7",
                  DTLZ.DTLZ7F(numObjectives, numDecisionVariables),
                  dtlzBounds,
                  ControlParameters(swarms)),
      )
    )
  }

  def zdtObj(numObjectives: Int, swarms: NonEmptyList[Int]): BenchmarkSuite = {
    val zdt1Bounds = Interval(0.0, 1.0) ^ 30
    val zdt2Bounds = Interval(0.0, 1.0) ^ 30
    val zdt3Bounds = Interval(0.0, 1.0) ^ 30
    val zdt4Bounds = Interval(0.0, 1.0) <:: (Interval(-5.0, 5.0) ^ 9)
    val zdt6Bounds = Interval(0.0, 1.0) ^ 10

    BenchmarkSuite(
      "ZDT",
      NonEmptyList(
        Benchmark("ZDT1", ZDT.ZDT1F, zdt1Bounds, ControlParameters(swarms)),
        Benchmark("ZDT2", ZDT.ZDT2F, zdt2Bounds, ControlParameters(swarms)),
        Benchmark("ZDT3", ZDT.ZDT3F, zdt3Bounds, ControlParameters(swarms)),
        Benchmark("ZDT4", ZDT.ZDT4F, zdt4Bounds, ControlParameters(swarms)),
        Benchmark("ZDT6", ZDT.ZDT6F, zdt6Bounds, ControlParameters(swarms))
      )
    )
  }
}
