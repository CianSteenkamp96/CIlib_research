package cilib.research.core

import cilib.research.core.GetIndices.{fitnessFromFreq, get3Indices, probFromFitness}
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
      case Empty(_, _) | EmptyPD(_, _, _, _) =>
        List()
      case NonEmpty(l, _, _) => l
      case NonEmptyPD(l, _, _, _, _) =>
        l
    }

  def bound: ArchiveBound =
    this match {
      case Empty(b, _) => b
      case EmptyPD(b, _, _, _) =>
        b
      case NonEmpty(_, b, _) => b
      case NonEmptyPD(_, b, _, _, _) =>
        b
    }

  def insert(v: A): Archive[A] =
    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case EmptyPD(b, insertPolicy, freqs, i123) =>
        NonEmptyPD(List(v), b, insertPolicy, freqs, i123)
      case NonEmpty(l, b, c) =>
        b match {
          case Bounded(limit, deletePolicy) =>
            // l.forall(x => !c(x, v)) means that there is no element in the list that dominates v
            if (l.size < limit.value && l.forall(x => !c(x, v))) {
              removeDominatedAndInsert(v)
            } else if (l.size == limit.value && l.forall(x => !c(x, v))) {
              val selected = deletePolicy(l)
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
        val updated_freqs_and_indices = update_freqs_and_indices(freqs)
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
              val selected = deletePolicy(l)
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
      case Empty(_, _) | NonEmpty(_, _, _) =>
        throw new Exception(
          "update_freqs_and_indices only used by PMGPSO (EmptyPD and NonEmptyPD archives) - Not relevant for MGPSO (Empty and NonEmpty archives).")
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY MGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  protected def removeDominatedAndInsert(v: A): Archive[A] =
    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case NonEmpty(l, b, c) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        NonEmpty[A](v :: l.filterNot(dominated.contains), b, c)
      case EmptyPD(_, _, _, _) | NonEmptyPD(_, _, _, _, _) =>
        throw new Exception(
          "removeDominatedAndInsert only used by MGPSO (Empty and NonEmpty archives) - Not relevant for PMGPSO (EmptyPD and NonEmptyPD archives).")
    }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY MGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def removeDominated(v: A, l: List[A]): List[A] =
    this match {
      case Empty(_, _) => List[A]()
      case NonEmpty(_, _, c) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        l.filterNot(dominated.contains)
      case EmptyPD(_, _, _, _) | NonEmptyPD(_, _, _, _, _) =>
        throw new Exception(
          "removeDominatedAndInsert only used by MGPSO (Empty and NonEmpty archives) - Not relevant for PMGPSO (EmptyPD and NonEmptyPD archives).")
    }

  def empty: Archive[A] =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) =>
        this
      case NonEmpty(_, b, c) => Empty(b, c)
      case NonEmptyPD(_, b, insertPolicy, freqs, i123) =>
        EmptyPD(b, insertPolicy, freqs, i123)
    }

  def isEmpty: Boolean =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) =>
        true
      case NonEmpty(_, _, _) | NonEmptyPD(_, _, _, _, _) =>
        false
    }

  def size: Int =
    this match {
      case Empty(_, _) | EmptyPD(_, _, _, _) =>
        0
      case NonEmpty(l, _, _) => l.size
      case NonEmptyPD(l, _, _, _, _) =>
        l.size

    }
}

object Archive {
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

  ////////////////////////////////////////////////// FOR PARTIAL-DOMINANCE MGPSO (PMGPSO) ARCHIVE ///////////////////////////////////////////////////////
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
}
