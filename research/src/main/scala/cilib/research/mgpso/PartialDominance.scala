//package cilib.research.mgpso
//
//import scalaz._
//import cilib.research.core.GetIndices._
//
//case class PartialDominance(freqs: NonEmptyList[Int], i123: (Int, Int, Int), normalMGPSO: Boolean) // ##################################################### NEW ####################################################################
//{
//		def set_randomIndices_and_updateFreqs: PartialDominance = {
//			val probs: NonEmptyList[Double] = probFromFitness(fitnessFromFreq(freqs))
//			val randomIndices: (Int, Int, Int) = get3Indices(probs)
//				val newFreqs = freqs.zipWithIndex.map(el =>
//					if ((el._2 == randomIndices._1) || (el._2 == randomIndices._2) || (el._2 == randomIndices._3))
//						el._1 + 1
//					else el._1)
//			this.copy(freqs = newFreqs, i123 = randomIndices)
////			PartialDominance(newFreqs, randomIndices, normalMGPSO = false)
//  }
//}

package cilib.research.mgpso

import scalaz._
import cilib.research.core.GetIndices._

sealed abstract class PartialDominance {
	import PartialDominance._

	def set_randomIndices_and_updateFreqs: PartialDominance = {
		this match {
			case PartialDominanceCase(freqs, _, _) => {
				val probs: NonEmptyList[Double] = probFromFitness(fitnessFromFreq(freqs))
				val randomIndices: (Int, Int, Int) = get3Indices(probs)
				val newFreqs = freqs.zipWithIndex.map(el =>
					if ((el._2 == randomIndices._1) || (el._2 == randomIndices._2) || (el._2 == randomIndices._3))
						el._1 + 1
					else el._1)
				//			this.copy(freqs = newFreqs, i123 = randomIndices)
				PartialDominanceCase(newFreqs, randomIndices, false)
			}
		}
	}

	def getIndices: (Int, Int, Int) =
		this match {
			case PartialDominanceCase(_, i123, _) => i123
		}

	def normal: Boolean =
		this match {
			case PartialDominanceCase(_, _, normalMGPSO) => normalMGPSO
		}
}

object PartialDominance {
	private final case class PartialDominanceCase(freqs: NonEmptyList[Int], i123: (Int, Int, Int), normalMGPSO: Boolean)
		extends PartialDominance

	def partialDominance(freqs: NonEmptyList[Int], i123: (Int, Int, Int), normalMGPSO: Boolean): PartialDominance = PartialDominanceCase(freqs, i123, normalMGPSO)
}
