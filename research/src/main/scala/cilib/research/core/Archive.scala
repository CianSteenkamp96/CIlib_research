// Cian Steenkamp & Kyle Erwin (and Gary Pampara)

package cilib.research.core

import cilib.research.core.GetIndices.{fitnessFromFreq, get3Indices, probFromFitness}
import cilib.research.mgpso.{CrowdingDistance, KneePoint, MGParticle}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import scalaz.Scalaz._
import scalaz._

sealed trait ArchiveBound
final case class Bounded[A](limit: Int Refined Positive, deletePolicy: List[A] => A)
    extends ArchiveBound
final case class Unbounded() extends ArchiveBound

// In our case
// l is all our solutions
// b is our capacity of 50 with a crowding distance delete policy
// c is a our Dominates insert policy so
//  c(v, x) means "does v dominate x"
//  !c(v, x) means "does v not dominate x"

sealed abstract class Archive[A] {
  import Archive._

  def values: List[A] =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) | EmptyKP(_, _, _, _, _) => List()
      case NonEmpty(l, _, _)                                          => l
      case NonEmptyPD(l, _, _, _, _)                                  => l
      case NonEmptyKP(l, _, _, _, _, _)                               => l
    }

  def bound: ArchiveBound =
    this match {
      case Empty(b, _)                  => b
      case EmptyPD(b, _, _, _)          => b
      case EmptyKP(b, _, _, _, _)       => b
      case NonEmpty(_, b, _)            => b
      case NonEmptyPD(_, b, _, _, _)    => b
      case NonEmptyKP(_, b, _, _, _, _) => b
    }

  def insert(v: A): Archive[A] =
    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case EmptyPD(b, insertPolicy, freqs, i123) =>
        NonEmptyPD(List(v), b, insertPolicy, freqs, i123)
      case EmptyKP(b, insertPolicy, dr, pr_prKPs2ND, _R) =>
        NonEmptyKP(List(v), b, insertPolicy, dr, pr_prKPs2ND, _R)
      case NonEmpty(l, b, c) =>
        b match {
          case Bounded(limit, deletePolicy) =>
            // l.forall(x => !c(x, v)) means that there is no element in the list that dominates v
            if (l.size < limit.value && l.forall(x => !c(x, v))) {
              removeDominatedAndInsert(v)
            } else if (l.size == limit.value && l.forall(x => !c(x, v))) {
              val selected = deletePolicy(l) // HERE KYLE ???
              NonEmpty[A](l.filterNot(x => x.equals(selected)), b, c).removeDominatedAndInsert(v)
            } else
              NonEmpty[A](l, b, c)
          case Unbounded() =>
            if (l.forall(x => !c(x, v)))
              removeDominatedAndInsert(v)
            else
              NonEmpty[A](l, b, c)
        }
      case NonEmptyPD(l, b, c, freqs, _) =>
        val updated_freqs_and_indices = update_freqs_and_indices(freqs) // think this should/could be moved into the ifs below to ensure updates only for successful inserts
        b match {
          case Bounded(limit, deletePolicy) =>
            if (l.size < limit.value && l.forall(x => !c(x, v, updated_freqs_and_indices._2))) {
              NonEmptyPD[A](v :: l,
                            b,
                            c,
                            updated_freqs_and_indices._1,
                            updated_freqs_and_indices._2)
            } else if (l.size == limit.value && l.forall(x =>
                         !c(x, v, updated_freqs_and_indices._2))) {
              val selected = deletePolicy(l) // HERE KYLE ???
              NonEmptyPD[A](v :: l.filterNot(x => x.equals(selected)),
                            b,
                            c,
                            updated_freqs_and_indices._1,
                            updated_freqs_and_indices._2)
            } else
              NonEmptyPD[A](l, b, c, updated_freqs_and_indices._1, updated_freqs_and_indices._2) // Note: freqs and indices updated even if insert fails
          case Unbounded() =>
            if (l.forall(x => !c(x, v, updated_freqs_and_indices._2)))
              NonEmptyPD[A](v :: l,
                            b,
                            c,
                            updated_freqs_and_indices._1,
                            updated_freqs_and_indices._2)
            else
              NonEmptyPD[A](l, b, c, updated_freqs_and_indices._1, updated_freqs_and_indices._2) // Note: freqs and indices updated even if insert fails
        }
      case NonEmptyKP(l, b, c, dr, pr_prKPs2ND, _R) =>
        b match {
          case Bounded(limit, deletePolicy) =>
            // l.forall(x => !c(x, v)) means that there is no element in the list that dominates v
            if (l.size < limit.value && l.forall(x => !c(x, v)))
              removeDominatedAndInsert(v).update_KP_neighbourhoods
            else if (l.size == limit.value && l.forall(x => !c(x, v))) {
              val selected = deletePolicy(l) // HERE KYLE ???
              NonEmptyKP[A](l.filterNot(x => x.equals(selected)), b, c, dr, pr_prKPs2ND, _R)
                .removeDominatedAndInsert(v)
                .update_KP_neighbourhoods
            } else
              NonEmptyKP[A](l, b, c, dr, pr_prKPs2ND, _R)
          case Unbounded() =>
            if (l.forall(x => !c(x, v)))
              removeDominatedAndInsert(v).update_KP_neighbourhoods
            else
              NonEmptyKP[A](l, b, c, dr, pr_prKPs2ND, _R)
        }
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY PMGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def update_freqs_and_indices(freqs: NonEmptyList[Int]): (NonEmptyList[Int], (Int, Int, Int)) =
    this match {
      case EmptyPD(_, _, _, _) | NonEmptyPD(_, _, _, _, _) =>
        val probs: NonEmptyList[Double] = probFromFitness(fitnessFromFreq(freqs))
        val randomIndices: (Int, Int, Int) = get3Indices(probs)
        val newFreqs = freqs.zipWithIndex.map(el =>
          if ((el._2 == randomIndices._1) || (el._2 == randomIndices._2) || (el._2 == randomIndices._3))
            el._1 + 1
          else el._1)
        (newFreqs, randomIndices)
      case Empty(_, _) | NonEmpty(_, _, _) | EmptyKP(_, _, _, _, _) |
          NonEmptyKP(_, _, _, _, _, _) =>
        throw new Exception("update_freqs_and_indices only used by PMGPSO.")
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! NOT USED BY PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  protected def removeDominatedAndInsert(v: A): Archive[A] =
    this match {
      case Empty(b, c)                        => NonEmpty[A](List(v), b, c)
      case EmptyKP(b, c, dr, pr_prKPs2ND, _R) => NonEmptyKP[A](List(v), b, c, dr, pr_prKPs2ND, _R)
      case NonEmpty(l, b, c) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        NonEmpty[A](v :: l.filterNot(dominated.contains), b, c)
      case NonEmptyKP(l, b, c, dr, pr_prKPs2ND, _R) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        NonEmptyKP[A](v :: l.filterNot(dominated.contains), b, c, dr, pr_prKPs2ND, _R)
      case EmptyPD(_, _, _, _) | NonEmptyPD(_, _, _, _, _) =>
        throw new Exception("removeDominatedAndInsert - Irrelevant for PMGPSO.")
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! NOT USED BY PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def removeDominated(v: A, l: List[A]): List[A] =
    this match {
      case Empty(_, _) | EmptyKP(_, _, _, _, _) => List[A]()
      case NonEmpty(_, _, c) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        l.filterNot(dominated.contains)
      case NonEmptyKP(_, _, c, _, _, _) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        l.filterNot(dominated.contains)
      case EmptyPD(_, _, _, _) | NonEmptyPD(_, _, _, _, _) =>
        throw new Exception("removeDominatedAndInsert - Irrelevant for PMGPSO.")
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY KnMGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  // for future: how does KnPSO and KnEA cope with calc overhead/recalc ?
  // adaptive neighbourhood strategy for knee points identification
  def update_KP_neighbourhoods: Archive[A] =
    this match {
      case Empty(_, _) | NonEmpty(_, _, _) | EmptyPD(_, _, _, _) | NonEmptyPD(_, _, _, _, _) =>
        throw new Exception("update_KP_neighbourhoods - Only relevant for KnMGPSO.")
      // this should never be the case below
      case EmptyKP(b, c, dr, _, _R) =>
        EmptyKP[A](b, c, dr, (1.0, 0.0), _R) // starting values according to lit
      case NonEmptyKP(l, b, c, dr, pr_prKPs2ND, _) =>
        // empty archive means no KP calc - fall back to vanilla PSO velocity eq
        val fitnessValues: List[List[Double]] = l.map(x =>
          x match {
            case x.getClass == MGParticle =>
              x.pos.fitness.toList // thought 'pattern matching' would ensure x.pos to be visible/available ???
        }) // HERE KYLE ???
        val numObjectives: Int = fitnessValues.head.size

        // ratio of the neighbourhood size to the range spanned by objective m at iteration t
        val ratio: Double = pr_prKPs2ND._1 * math.pow(math.E,
                                                      -(1 - (pr_prKPs2ND._2 / dr)) / numObjectives)
        // max for each objective
        val maxes: List[Double] = fitnessValues.transpose.map(_.max)
        // min for each objective
        val mins: List[Double] = fitnessValues.transpose.map(_.min)
        // size of neighbourhood for each objective
        val new__R: NonEmptyList[Double] = (maxes, mins).zipped.map(_ - _).map(_ * ratio).toNel.get
        assert(new__R.size == numObjectives)

        val numKPs: Int = l.foldLeft(0)((acc, s) => {
          if (s == KneePoint
                .kneePoint(l.toNel.get, new__R) // HERE KYLE ???
                .getOrElse(CrowdingDistance.leastCrowded(l.take(2)))) // HERE KYLE ???
            acc + 1
          else acc
        })

        val new_pr_prKPs2ND = (ratio, (numKPs / l.size) * 1.0)

        NonEmptyKP[A](l, b, c, dr, new_pr_prKPs2ND, new__R)
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY KnMGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def get_pr_prKPs2ND: (Double, Double) =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) | NonEmpty(_, _, _) | NonEmptyPD(_, _, _, _, _) =>
        throw new Exception("get_pr_prKPs2ND - Only relevant for KnMGPSO.")
      case EmptyKP(_, _, _, pr_prKPs2ND, _)       => pr_prKPs2ND
      case NonEmptyKP(_, _, _, _, pr_prKPs2ND, _) => pr_prKPs2ND
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY KnMGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def get_dr: Double =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) | NonEmpty(_, _, _) | NonEmptyPD(_, _, _, _, _) =>
        throw new Exception("get_dr - Only relevant for KnMGPSO.")
      case EmptyKP(_, _, dr, _, _)       => dr
      case NonEmptyKP(_, _, _, dr, _, _) => dr
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY KnMGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def get_R: NonEmptyList[Double] =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) | NonEmpty(_, _, _) | NonEmptyPD(_, _, _, _, _) => NonEmptyList(-1.0)
      case EmptyKP(_, _, _, _, _R)       => _R
      case NonEmptyKP(_, _, _, _, _, _R) => _R
    }

  def empty: Archive[A] =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) | EmptyKP(_, _, _, _, _) => this
      case NonEmpty(_, b, c)                                          => Empty(b, c)
      case NonEmptyPD(_, b, insertPolicy, freqs, i123)                => EmptyPD(b, insertPolicy, freqs, i123)
      case NonEmptyKP(_, b, insertPolicy, dr, pr_prKPs2ND, _R) =>
        EmptyKP(b, insertPolicy, dr, pr_prKPs2ND, _R)
    }

  def isEmpty: Boolean =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) | EmptyKP(_, _, _, _, _)                   => true
      case NonEmpty(_, _, _) | NonEmptyPD(_, _, _, _, _) | NonEmptyKP(_, _, _, _, _, _) => false
    }

  def size: Int =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) | EmptyKP(_, _, _, _, _) => 0
      case NonEmpty(l, _, _)                                          => l.size
      case NonEmptyPD(l, _, _, _, _)                                  => l.size
      case NonEmptyKP(l, _, _, _, _, _)                               => l.size
    }
}

object Archive {
  ////////////////////////////////////////////////// MGPSO ARCHIVE ///////////////////////////////////////////////////////
  private final case class Empty[A](b: ArchiveBound, insertPolicy: (A, A) => Boolean)
      extends Archive[A]

  private final case class NonEmpty[A](l: List[A], b: ArchiveBound, insertPolicy: (A, A) => Boolean)
      extends Archive[A]

  def bounded[A](limit: Int Refined Positive,
                 insertPolicy: (A, A) => Boolean,
                 deletePolicy: List[A] => A): Archive[A] =
    Empty[A](Bounded(limit, deletePolicy), insertPolicy)

  def unbounded[A](insertPolicy: (A, A) => Boolean): Archive[A] =
    Empty[A](Unbounded(), insertPolicy)

  def boundedNonEmpty[A](seeds: NonEmptyList[A],
                         limit: Int Refined Positive,
                         insertPolicy: (A, A) => Boolean,
                         deletePolicy: List[A] => A): Archive[A] = {
    val emptyArchive: Archive[A] = bounded(limit, insertPolicy, deletePolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }

  def unboundedNonEmpty[A](seeds: NonEmptyList[A], insertPolicy: (A, A) => Boolean): Archive[A] = {
    val emptyArchive: Archive[A] = unbounded(insertPolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }

  ////////////////////////////////////////////////// PMGPSO ARCHIVE ///////////////////////////////////////////////////////
  // Empty Archive using Partial-Dominance as insertPolicy
  private final case class EmptyPD[A](b: ArchiveBound,
                                      insertPolicy: (A, A, (Int, Int, Int)) => Boolean,
                                      freqs: NonEmptyList[Int],
                                      i123: (Int, Int, Int))
      extends Archive[A]

  // NonEmpty Archive using Partial-Dominance as insertPolicy
  private final case class NonEmptyPD[A](l: List[A],
                                         b: ArchiveBound,
                                         insertPolicy: (A, A, (Int, Int, Int)) => Boolean,
                                         freqs: NonEmptyList[Int],
                                         i123: (Int, Int, Int))
      extends Archive[A]

  def boundedPD[A](limit: Int Refined Positive,
                   insertPolicy: (A, A, (Int, Int, Int)) => Boolean,
                   deletePolicy: List[A] => A,
                   freqs: NonEmptyList[Int],
                   i123: (Int, Int, Int)): Archive[A] =
    EmptyPD[A](Bounded(limit, deletePolicy), insertPolicy, freqs, i123)

  def unboundedPD[A](insertPolicy: (A, A, (Int, Int, Int)) => Boolean,
                     freqs: NonEmptyList[Int],
                     i123: (Int, Int, Int)): Archive[A] =
    EmptyPD[A](Unbounded(), insertPolicy, freqs, i123)

  def boundedNonEmptyPD[A](seeds: NonEmptyList[A],
                           limit: Int Refined Positive,
                           insertPolicy: (A, A, (Int, Int, Int)) => Boolean,
                           deletePolicy: List[A] => A,
                           freqs: NonEmptyList[Int],
                           i123: (Int, Int, Int)): Archive[A] = {
    val emptyArchive: Archive[A] = boundedPD(limit, insertPolicy, deletePolicy, freqs, i123)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }

  def unboundedNonEmptyPD[A](seeds: NonEmptyList[A],
                             insertPolicy: (A, A, (Int, Int, Int)) => Boolean,
                             freqs: NonEmptyList[Int],
                             i123: (Int, Int, Int)): Archive[A] = {
    val emptyArchive: Archive[A] = unboundedPD(insertPolicy, freqs, i123)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }

  ////////////////////////////////////////////////// KnMGPSO ARCHIVE ///////////////////////////////////////////////////////
  // Empty Archive using Knee Points for archive guide selection
  private final case class EmptyKP[A](b: ArchiveBound,
                                      insertPolicy: (A, A) => Boolean,
                                      // desired_ratio_KPs_2_ND_sols
                                      dr: Double,
                                      // prev_ratio and prev_ratio_KPs_2_ND_sols used for adaptive neighbourhoods of KP calculation
                                      pr_prKPs2ND: (Double, Double),
                                      // neighbourhood size per objective
                                      _R: NonEmptyList[Double])
      extends Archive[A]

  // NonEmpty Archive using Knee Points for archive guide selection
  private final case class NonEmptyKP[A](l: List[A],
                                         b: ArchiveBound,
                                         insertPolicy: (A, A) => Boolean,
                                         // desired_ratio_KPs_2_ND_sols
                                         dr: Double,
                                         // prev_ratio and prev_ratio_KPs_2_ND_sols used for adaptive neighbourhoods of KP calculation
                                         pr_prKPs2ND: (Double, Double),
                                         // neighbourhood size per objective
                                         _R: NonEmptyList[Double])
      extends Archive[A]

  def boundedKP[A](limit: Int Refined Positive,
                   insertPolicy: (A, A) => Boolean,
                   deletePolicy: List[A] => A,
                   dr: Double,
                   pr_prKPs2ND: (Double, Double),
                   // neighbourhood size per objective
                   _R: NonEmptyList[Double]): Archive[A] =
    EmptyKP[A](Bounded(limit, deletePolicy), insertPolicy, dr, pr_prKPs2ND, _R)

  def unboundedKP[A](insertPolicy: (A, A) => Boolean,
                     dr: Double,
                     pr_prKPs2ND: (Double, Double),
                     // neighbourhood size per objective
                     _R: NonEmptyList[Double]): Archive[A] =
    EmptyKP[A](Unbounded(), insertPolicy, dr, pr_prKPs2ND, _R)

  def boundedNonEmptyKP[A](seeds: NonEmptyList[A],
                           limit: Int Refined Positive,
                           insertPolicy: (A, A) => Boolean,
                           deletePolicy: List[A] => A,
                           dr: Double,
                           pr_prKPs2ND: (Double, Double),
                           // neighbourhood size per objective
                           _R: NonEmptyList[Double]): Archive[A] = {
    val emptyArchive: Archive[A] = boundedKP(limit, insertPolicy, deletePolicy, dr, pr_prKPs2ND, _R)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }

  def unboundedNonEmptyKP[A](seeds: NonEmptyList[A],
                             insertPolicy: (A, A) => Boolean,
                             dr: Double,
                             pr_prKPs2ND: (Double, Double),
                             // neighbourhood size per objective
                             _R: NonEmptyList[Double]): Archive[A] = {
    val emptyArchive: Archive[A] = unboundedKP(insertPolicy, dr, pr_prKPs2ND, _R)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }
}
