package cilib.research.benchmarks.wfg

object Assert {

  def apply(x: Boolean): Option[Boolean] =
    if (x) Some(x) else None

}
