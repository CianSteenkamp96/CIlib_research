package cilib.research.core
import cilib.research.benchmarks.wfg.WFG._
import cilib.research.benchmarks.zdt.ZDT._
import cilib.research.benchmarks.zdt._
import scalaz.NonEmptyList

case class BenchmarkSuite(name: String, benchmarks: NonEmptyList[Benchmark])

object BenchmarkSuite {

  val ZDT = BenchmarkSuite(
    "ZDT",
    NonEmptyList(
      Benchmark("ZDT1",
                ZDT1F,
                ZDT1.bounds,
                ControlParameters(0.475, 1.80, 1.10, 1.80, NonEmptyList(33, 17))),
      Benchmark("ZDT2",
                ZDT2F,
                ZDT2.bounds,
                ControlParameters(0.075, 1.60, 1.35, 1.90, NonEmptyList(8, 42))),
      Benchmark("ZDT3",
                ZDT3F,
                ZDT3.bounds,
                ControlParameters(0.050, 1.85, 1.90, 1.90, NonEmptyList(8, 42))),
      Benchmark("ZDT4",
                ZDT4F,
                ZDT4.bounds,
                ControlParameters(0.175, 1.85, 1.35, 1.85, NonEmptyList(5, 45))),
      Benchmark("ZDT6",
                ZDT6F,
                ZDT6.bounds,
                ControlParameters(0.600, 1.85, 1.55, 1.80, NonEmptyList(1, 49)))
    )
  )

  val WFG_2D = BenchmarkSuite(
    "WFG.2D",
    NonEmptyList(
      Benchmark("WFG1.2D",
                WFG1(2),
                bounds,
                ControlParameters(0.275, 1.65, 1.80, 1.75, NonEmptyList(45, 5))),
      Benchmark("WFG2.2D",
                WFG2(2),
                bounds,
                ControlParameters(0.750, 1.15, 1.70, 1.05, NonEmptyList(24, 26))),
      Benchmark("WFG3.2D",
                WFG3(2),
                bounds,
                ControlParameters(0.600, 1.60, 1.85, 0.95, NonEmptyList(31, 19))),
      Benchmark("WFG4.2D",
                WFG4(2),
                bounds,
                ControlParameters(0.100, 0.80, 1.65, 1.70, NonEmptyList(2, 48))),
      Benchmark("WFG5.2D",
                WFG5(2),
                bounds,
                ControlParameters(0.600, 0.80, 1.60, 1.85, NonEmptyList(50, 0))),
      Benchmark("WFG6.2D",
                WFG6(2),
                bounds,
                ControlParameters(0.525, 0.65, 0.60, 1.65, NonEmptyList(19, 31))),
      Benchmark("WFG7.2D",
                WFG7(2),
                bounds,
                ControlParameters(0.450, 1.20, 1.85, 1.55, NonEmptyList(29, 21))),
      Benchmark("WFG8.2D",
                WFG8(2),
                bounds,
                ControlParameters(0.750, 1.00, 1.65, 1.05, NonEmptyList(37, 13))),
      Benchmark("WFG9.2D",
                WFG9(2),
                bounds,
                ControlParameters(0.275, 1.00, 0.50, 1.70, NonEmptyList(13, 37)))
    )
  )

  val OnlyWFG6_2D = BenchmarkSuite(
    "WFG6.2D",
    NonEmptyList(
      Benchmark("WFG6.2D.All",
                WFG6(2),
                bounds,
                ControlParameters(0.525, 0.65, 0.60, 1.65, NonEmptyList(19, 31)))
    )
  )

  val WFG_3D = BenchmarkSuite(
    "WFG.3D",
    NonEmptyList(
      Benchmark("WFG1.3D",
                WFG1(3),
                bounds,
                ControlParameters(0.125, 1.20, 1.30, 1.75, NonEmptyList(37, 4, 9))),
      Benchmark("WFG2.3D",
                WFG2(3),
                bounds,
                ControlParameters(0.275, 1.25, 1.40, 1.70, NonEmptyList(24, 25, 1))),
      Benchmark("WFG3.3D",
                WFG3(3),
                bounds,
                ControlParameters(0.525, 1.65, 1.75, 0.75, NonEmptyList(29, 10, 11))),
      Benchmark("WFG4.3D",
                WFG4(3),
                bounds,
                ControlParameters(0.275, 1.75, 0.50, 1.05, NonEmptyList(29, 21, 0))),
      Benchmark("WFG5.3D",
                WFG5(3),
                bounds,
                ControlParameters(0.575, 0.60, 1.85, 1.75, NonEmptyList(2, 48, 0))),
      Benchmark("WFG6.3D",
                WFG6(3),
                bounds,
                ControlParameters(0.300, 0.90, 0.90, 1.90, NonEmptyList(5, 30, 15))),
      Benchmark("WFG7.3D",
                WFG7(3),
                bounds,
                ControlParameters(0.425, 1.45, 1.50, 1.40, NonEmptyList(10, 22, 18))),
      Benchmark("WFG8.3D",
                WFG8(3),
                bounds,
                ControlParameters(0.425, 0.95, 1.75, 1.85, NonEmptyList(4, 23, 23))),
      Benchmark("WFG9.3D",
                WFG9(3),
                bounds,
                ControlParameters(0.275, 1.25, 0.75, 1.50, NonEmptyList(4, 45, 1)))
    )
  )
}
