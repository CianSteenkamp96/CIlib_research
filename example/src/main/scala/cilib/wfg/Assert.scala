package cilib
package wfg

object Assert {

  def apply(x: Boolean): Option[Boolean] =
    if (x) Some(x) else None

}
