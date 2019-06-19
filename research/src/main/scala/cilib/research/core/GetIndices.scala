// Cian Steenkamp

// Note: Still some exceptions that might have to be removed - functional programming ! ! ! ! ! !

package cilib.research.core
import scalaz._
import Scalaz._
import cilib.{RNG, RVar}

object GetIndices {

// Assign 'fitness' vals based on frequency chosen
  def fitnessFromFreq(freqs: NonEmptyList[Int]): NonEmptyList[Double] = {
    val total = freqs.suml

    if (total == 0 || freqs.toList.forall(_ == freqs.head)) // if all objectives have been chosen 0 times OR a equal number of times => dws equal prob
      freqs.map(_ => 1.0 / freqs.size)
    else
      freqs.map(f => 1 - (f.toDouble / total)) // So that more popular objectives will be less likely to be picked and unpopular (low freq) will have a better chance of being chosen
  }

  // Assign a probability based on fitness vals
  def probFromFitness(probs: NonEmptyList[Double]): NonEmptyList[Double] = {
    val total = probs.foldLeft(0.0)(_ + _)

    if (total == 0)
      probs.map(_ => 1.0 / probs.size)
    else
      probs.map(f => f / total)
  }

  // Returns 3 indices/objectives to be considered for diminance relation (partial-dominance)
  // This function prioritizes the indices/objectives with the largest probabilities (least chosen objectives) even more...
  // Since, only one pick is made to save time - previous versions made 3 picks and had a lot of repititions to ensure 3 unique indices
  // This implementation makes one pick, gets the index, and then chooses the other 2 indices as the 2 indices left of the chosen index (extra prioritization of lesser chosen objectives/indices)
  def get3Indices(probs: NonEmptyList[Double]): (Int, Int, Int) =
    if (probs.size == 3) // check for this before calculating probabilities (calling the above funcs) etc. - save some comp time
      (0, 1, 2) // 3 objectives -> use indices 0, 1, 2
    else if (probs.size > 3) {
      val pick = RVar.doubles(1).run(RNG.fromTime)._2.head // 0 < pick < 1
      val sortedProbs = probs.zipWithIndex.sortWith(_ > _) // individual probs wih indices sorted
      val stick = sortedProbs.toList
        .scanLeft(0.0)((acc, el) => acc + el._1)
        .drop(1)
        .zip(sortedProbs.map(el => el._2).toList) // cumulative sorted probs with indices // drop is used to remove start of the 'stick' (0.0)

      // i123
      val indexIdentification = stick
        .map(
          p =>
            if ((pick <= p._1) && (pick > 0) && (pick < 1))
              Some(p._2)
            else
            None)
        .filter(_.isDefined)
        if (indexIdentification.empty) throw new Exception("Error: pick did not satisfy any part of the stick.") // (stick.head._2, stick(1)._2, stick(2)._2) // should never be the case - just for safety
        else if(indexIdentification.head.get == stick.reverse.head._2) (stick.head._2, stick(1)._2, stick.reverse.head._2) // if pick satisfies last part of stick
        else if((indexIdentification.head.get == stick.head._2) || (indexIdentification.head.get == stick(1)._2) || (indexIdentification.head.get == stick(2)._2)) (stick.head._2, stick(1)._2, stick(2)._2) // if pick satisfies 1st, 2nd or 3rd part of stick from the front
        else {
          val i = stick.zipWithIndex.filter(_._1._2 == indexIdentification.head.get).head._2
          (indexIdentification.head.get, stick(i - 1)._2, stick(i - 2)._2)
        } // if pick satisfies part of stick in the middle (not 1st, 2nd, or 3rd, or last)
    }
    else throw new Exception("Error: NonEmptyList of probabilities size < 3.")
}