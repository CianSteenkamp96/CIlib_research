package cilib
package exec

final case class Info(alg: String,
                                prob: String,
                                iteration: Int,
                                env: Env,
                                seed: Long)
