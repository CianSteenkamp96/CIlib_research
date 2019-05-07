package cilib.research.core

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import scalaz.Scalaz._
import scalaz._
import GetIndices._

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
      case Empty(_, _) | PartialEmpty(_, _, _)  => List()
      case NonEmpty(l, _, _)                    => l
      case PartialNonEmpty(l , _, _, _)         => l
    }

  def bound: ArchiveBound =
    this match {
      case Empty(b, _)                  => b
      case PartialEmpty(b, _, _)        => b
      case NonEmpty(_, b, _)            => b
      case PartialNonEmpty(_, b, _, _)  => b
    }

// Unnecessary according to Gary
//  def insertCondition: (A, A) => Boolean =
//    this match {
//      case Empty(_, c)       => c
//      case NonEmpty(_, _, c) => c
//    }

  def insert(v: A): Archive[A] =
    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case PartialEmpty(b, c, f) => PartialNonEmpty[A](List(v), b, c, f)
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
      case PartialNonEmpty(l, b, c, f) => {
        val probs: NonEmptyList[Double] = probFromFitness(fitnessFromFreq(f))
        val random_indices = get3Indices(probs)

        b match {
          case Bounded(limit, deletePolicy) =>
            if (l.size < limit.value && l.forall(current => !c(current, v, random_indices))) {
              PartialNonEmpty[A](v :: l, b, c, updateFreqs(f, random_indices))
            } else if (l.size == limit.value && l.forall(current => !c(current, v, random_indices))) {
              val selected = deletePolicy(l)
              PartialNonEmpty[A](v :: l.filterNot(x => x.equals(selected)), b, c, updateFreqs(f, random_indices))
            } else
              PartialNonEmpty[A](l, b, c, updateFreqs(f, random_indices))
          case Unbounded() =>
            if (l.forall(current => !c(current, v, random_indices)))
              PartialNonEmpty[A](v :: l, b, c, updateFreqs(f, random_indices))
            else
              PartialNonEmpty[A](l, b, c, updateFreqs(f, random_indices))
        }
      }
    }

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  protected def removeDominatedAndInsert(v: A): Archive[A] =
    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case PartialEmpty(b, c, f) => PartialEmpty[A](b, c, f) // should never be called in reality for PartialEmpty case
      case NonEmpty(l, b, c) =>
        val dominated = l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        NonEmpty[A](v :: l.filterNot(dominated.contains), b, c)
      case PartialNonEmpty(l, b, insertPolicy, frequencies) => PartialNonEmpty[A](l, b, insertPolicy, frequencies) // should never be called in reality for PartialNonEmpty case
    }

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def removeDominated(v: A, l: List[A]): List[A] =
    this match {
      case Empty(_, _) | PartialEmpty(_, _, _) => List[A]() // should never be called in reality for PartialEmpty case
      case NonEmpty(_, _, c) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        l.filterNot(dominated.contains)
      case PartialNonEmpty(l, _, _, _) => l // should never be called in reality for PartialEmpty case
    }

  def empty: Archive[A] =
    this match {
      case Empty(_, _) | PartialEmpty(_, _, _) => this
      case NonEmpty(_, b, c) => Empty(b, c)
      case PartialNonEmpty(_, b, c, f) => PartialEmpty(b, c, f)
    }

  def isEmpty: Boolean =
    this match {
      case Empty(_, _) | PartialEmpty(_, _, _) => true
      case NonEmpty(_, _, _) | PartialNonEmpty(_, _, _, _) => false
    }

  def size: Int =
    this match {
      case Empty(_, _) | PartialEmpty(_, _, _) => 0
      case NonEmpty(l, _, _) => l.size
      case PartialNonEmpty(l, _, _, _) => l.size
    }
}

object Archive {
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    private final case class Empty[A](b: ArchiveBound, insertPolicy: (A, A) => Boolean) extends Archive[A]
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    private final case class PartialEmpty[A](b: ArchiveBound, insertPolicy: (A, A, (Int, Int, Int)) => Boolean, frequencies: NonEmptyList[Int]) extends Archive[A]

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    private final case class NonEmpty[A](l: List[A], b: ArchiveBound, insertPolicy: (A, A) => Boolean) extends Archive[A]
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    private final case class PartialNonEmpty[A](l: List[A], b: ArchiveBound, insertPolicy: (A, A, (Int, Int, Int)) => Boolean, frequencies: NonEmptyList[Int]) extends Archive[A]

  // Let user create bounded instance - Gary ?
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def bounded[A](limit: Int Refined Positive,
                 insertPolicy: (A, A) => Boolean,
                 deletePolicy: List[A] => A): Archive[A] =
    Empty[A](Bounded(limit, deletePolicy), insertPolicy)

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def partialBounded[A](limit: Int Refined Positive,
                        insertPolicy: (A, A, (Int, Int, Int)) => Boolean,
                        deletePolicy: List[A] => A,
                        frequencies: NonEmptyList[Int]): Archive[A] =
    PartialEmpty[A](Bounded(limit, deletePolicy), insertPolicy, frequencies)

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def unbounded[A](insertPolicy: (A, A) => Boolean): Archive[A] =
    Empty[A](Unbounded(), insertPolicy)

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def partialUnbounded[A](insertPolicy: (A, A, (Int, Int, Int)) => Boolean, frequencies: NonEmptyList[Int]): Archive[A] =
    PartialEmpty[A](Unbounded(), insertPolicy, frequencies)

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def boundedNonEmpty[A](seeds: NonEmptyList[A],
                         limit: Int Refined Positive,
                         insertPolicy: (A, A) => Boolean,
                         deletePolicy: List[A] => A): Archive[A] = {
    val emptyArchive: Archive[A] = bounded(limit, insertPolicy, deletePolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def partialBoundedNonEmpty[A](seeds: NonEmptyList[A],
                                limit: Int Refined Positive,
                                insertPolicy: (A, A, (Int, Int, Int)) => Boolean,
                                deletePolicy: List[A] => A,
                                frequencies: NonEmptyList[Int]): Archive[A] = {
    val emptyArchive: Archive[A] = partialBounded(limit, insertPolicy, deletePolicy, frequencies)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def unboundedNonEmpty[A](seeds: NonEmptyList[A], insertPolicy: (A, A) => Boolean): Archive[A] = {
    val emptyArchive: Archive[A] = unbounded(insertPolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PMGPSO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def unboundedNonEmpty[A](seeds: NonEmptyList[A], insertPolicy: (A, A, (Int, Int, Int)) => Boolean, frequencies: NonEmptyList[Int]): Archive[A] = {
    val emptyArchive: Archive[A] = partialUnbounded(insertPolicy, frequencies)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(seed))
  }
}