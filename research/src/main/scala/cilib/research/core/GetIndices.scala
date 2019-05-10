// Cian Steenkamp
// Still to be improved - remove vars !!!!!!!!!!!!!

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
  def get3Indices(probs: NonEmptyList[Double]): (Int, Int, Int) =
    if (probs.size == 3) // check for this before calculating probabilities (calling above funcs) etc. - save some comp time
      (0, 1, 2) // 3 objectives -> use indices 0, 1, 2
    else {

      var pick = RVar.doubles(1).run(RNG.fromTime)._2.head // VAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      val sortedProbs = probs.zipWithIndex.sortWith(_ > _) // probs wih indices sorted
      val stick = sortedProbs.toList
        .scanLeft(0.0)((acc, el) => acc + el._1)
        .drop(1)
        .zip(sortedProbs.map(el => el._2).toList) // cumulative sorted probs with indices // drop is used to remove start of the 'stick' (0.0)

      // i1
      val indexIdentification1 = stick
        .map(
          p =>
            if ((pick <= p._1) && (pick > 0) && (pick < 1))
              Some(p._2)
            else
            None)
        .filter(_.isDefined)
      val i1 =
        if (indexIdentification1.empty) stick.reverse.head._2 else indexIdentification1.head.get

      // i2
      var i2 = -1 // VAR!!!!!!!!!!!!!!!!
      var validIndexChosen = false // VAR!!!!!!!!!!!!!!!!!
      while (!validIndexChosen) {
        pick = RVar.doubles(1).run(RNG.fromTime)._2.head
        val indexIdentification2 = stick
          .map(
            p =>
              if ((pick <= p._1) && (pick > 0) && (pick < 1))
                if (p._2 != i1)
                  Some(p._2)
                else
                  None
              else
              None)
          .filter(_.isDefined)
        i2 =
          if (indexIdentification2.empty) stick.reverse.head._2 else indexIdentification2.head.get
        if (i2 != i1)
          validIndexChosen = true
      }

      // i3
      var i3 = -1 // VAR!!!!!!!!!!!!!!!!
      validIndexChosen = false // VAR!!!!!!!!!!!!!!!!!
      while (!validIndexChosen) {
        pick = RVar.doubles(1).run(RNG.fromTime)._2.head
        val indexIdentification3 = stick
          .map(
            p =>
              if ((pick <= p._1) && (pick > 0) && (pick < 1))
                if ((p._2 != i1) && (p._2 != i2))
                  Some(p._2)
                else
                  None
              else
              None)
          .filter(_.isDefined)
        i3 =
          if (indexIdentification3.empty) stick.reverse.head._2 else indexIdentification3.head.get
        if ((i2 != i1) && (i3 != i1) && (i2 != i3))
          validIndexChosen = true
      }
      (i1, i2, i3) // return 3 indices as tuple
    }
}