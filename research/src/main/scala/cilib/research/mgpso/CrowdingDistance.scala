package cilib.research.mgpso

import java.lang.Math.abs

import cilib._
import scalaz.Scalaz._

object CrowdingDistance {
  def apply(collection: List[MGParticle], opt: Opt): MGParticle = {
    val fitnessValues = collection.map(x => x.pos.fitness.toList)
    // Returns NonEmptyList[List[Double]]
    val size = fitnessValues.head.size // Each list in the NonEmptyList will be the same size

    val result = for (i <- 0 until size) yield {
      val sorted =
        fitnessValues.zipWithIndex // We want to keep the the index to link each set of fitness values to it's Entity
          .map(x => (x._1(i), x._2)) // We are taking the ith fitness value // we are now at a List[Double, Int]
          .sortWith((x, y) => x._1 < y._1) // Sort the list based on the double value (fit value)

      val crowdingDistances = for (j <- sorted.indices)
        yield { // Calculate the crowing distance with respect to ith Objective
          val index = sorted(j)._2
          val distance =
            if (j == 0 || j == sorted.size - 1) Double.PositiveInfinity
            else abs(sorted(j - 1)._1 - sorted(j + 1)._1)
          (distance, index) // we still keep the index to link back to the original Entity
        }
      crowdingDistances // A list of crowding distances with respect to the ith objective
    } // A list of lists. Each inner list are the crowding distances

    val f = getOptFunction(opt)
    val zeroed = (for (_ <- 0 until fitnessValues.size) yield 0.0).toList

    val index =
      result.flatten // List[List[(crowding distance, entity index]] => List[(crowding distance, entity index]
        .foldLeft(zeroed)((a, c) => a.updated(c._2, a(c._2) + c._1)) // sum crowding distances based on index
        .zipWithIndex // we need the indexes again
        .sortWith((x, y) => f(x._1, y._1)) // Sort the list list based on the opt scheme
        .head
        ._2 // get the index

    collection(index)
  }

  private def getOptFunction(opt: Opt): (Double, Double) => Boolean =
    opt match {
      case Max =>
        (x: Double, y: Double) =>
          x > y
      case Min =>
        (x: Double, y: Double) =>
          x < y
    }

  def leastCrowded(collection: List[MGParticle]) = apply(collection, Max)
  def mostCrowded(collection: List[MGParticle]) = apply(collection, Min)
}
