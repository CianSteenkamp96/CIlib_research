package cilib.research.core
import cilib.research.core.GetIndices._
import scalaz.NonEmptyList

case class ControlParameters(
    w: Double,
    c1: Double,
    c2: Double,
    c3: Double,
    swarmSizes: NonEmptyList[Int],
    freqs: Option[(NonEmptyList[Int], (Int, Int, Int))] = None) // ##################################################### CHANGES ####################################################################
{
//  def setRandomIndices = this.copy(freqs = Some(this.freqs.get._1, (i123._1, i123._2, i123._3)))
  def set_randomIndices_and_updateFreqs
    : ControlParameters = // ##################################################### CHANGES ####################################################################
    if (freqs.isDefined) {
      val probs: NonEmptyList[Double] = probFromFitness(fitnessFromFreq(freqs.get._1))
      val randomIndices: (Int, Int, Int) = get3Indices(probs)
      this.copy(freqs = Some(
        (freqs.get._1.zipWithIndex.map(el =>
           if ((el._2 == randomIndices._1) || (el._2 == randomIndices._2) || (el._2 == randomIndices._3))
             el._1 + 1
           else el._1),
         randomIndices))) // Update doesn't work / changes does not persist???
    } else
      throw new Exception("Error: frequencies Option containing NonEmptyList of frequencies and 3 indices as Tuple is not defined.")
}