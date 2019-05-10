package cilib.research.core

import cilib.research.mgpso.Dominates
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
      case Empty(_, _)       => List()
      case NonEmpty(l, _, _) => l
    }

  def bound: ArchiveBound =
    this match {
      case Empty(b, _)       => b
      case NonEmpty(_, b, _) => b
    }

// Unnecessary according to Gary
//  def insertCondition: (A, A) => Boolean =
//    this match {
//      case Empty(_, c)       => c
//      case NonEmpty(_, _, c) => c
//    }

  def insert(envX: Benchmark)(v: A): Archive[A] =
    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case NonEmpty(l, b, c) =>
        if (!envX.controlParameters.freqs.isDefined)
          b match {
            case Bounded(limit, deletePolicy) =>
              // l.forall(x => !c(x, v)) means that there is no element in the list that dominates v
              if (l.size < limit.value && l.forall(x => !c(x, v)))
                removeDominatedAndInsert(v)
              else if (l.size == limit.value && l.forall(x => !c(x, v))) {
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
        else {
            envX.controlParameters.setRandomIndices_and_updateFreqs // ######################################### CHANGES / NEW ###########################################
            b match {
              case Bounded(limit, deletePolicy) =>
                if (l.size < limit.value && l.forall(current => !c(current, v)))
                  NonEmpty[A](v :: l, b, c)
//                  removeDominatedAndInsert(v) // #################### does cleanup make sense - pmgpso seems very SLOW - even with cleanup ..... ??????????????????????????????
                else if (l.size == limit.value && l.forall(current => !c(current, v))) {
                  val selected = deletePolicy(l)
                  NonEmpty[A](v :: l.filterNot(x => x.equals(selected)), b, c)
//                  NonEmpty[A](v :: l.filterNot(x => x.equals(selected)), b, c).removeDominatedAndInsert(v)
                } else
                  NonEmpty[A](l, b, c)
              case Unbounded() =>
                if (l.forall(current => !c(current, v)))
                  NonEmpty[A](v :: l, b, c)
//                  removeDominatedAndInsert(v)
                else
                  NonEmpty[A](l, b, c)
            }
        }
    }

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY MGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  protected def removeDominatedAndInsert(v: A): Archive[A] =
    this match {
      case Empty(b, c) => NonEmpty[A](List(v), b, c)
      case NonEmpty(l, b, c) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        NonEmpty[A](v :: l.filterNot(dominated.contains), b, c)
    }

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! USED BY MGPSO ONLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  def removeDominated(v: A, l: List[A]): List[A] =
    this match {
      case Empty(_, _) => List[A]()
      case NonEmpty(_, _, c) =>
        val dominated =
          l.foldLeft(List[A]())((acc, current) => if (c(v, current)) current :: acc else acc)
        l.filterNot(dominated.contains)
    }

  def empty: Archive[A] =
    this match {
      case Empty(_, _)       => this
      case NonEmpty(_, b, c) => Empty(b, c)
    }

  def isEmpty: Boolean =
    this match {
      case Empty(_, _)       => true
      case NonEmpty(_, _, _) => false
    }

  def size: Int =
    this match {
      case Empty(_, _)       => 0
      case NonEmpty(l, _, _) => l.size
    }
}

object Archive {
  private final case class Empty[A](b: ArchiveBound, insertPolicy: (A, A) => Boolean)
      extends Archive[A]

  private final case class NonEmpty[A](l: List[A], b: ArchiveBound, insertPolicy: (A, A) => Boolean)
      extends Archive[A]

  // Let user create bounded instance - Gary ?
  def bounded[A](limit: Int Refined Positive,
                 insertPolicy: (A, A) => Boolean,
                 deletePolicy: List[A] => A): Archive[A] =
    Empty[A](Bounded(limit, deletePolicy), insertPolicy)

  def unbounded[A](insertPolicy: (A, A) => Boolean): Archive[A] =
    Empty[A](Unbounded(), insertPolicy)

  def boundedNonEmpty[A](envX: Benchmark)(
      seeds: NonEmptyList[A], // ##################################### CHANGES #######################################
      limit: Int Refined Positive,
      insertPolicy: (A, A) => Boolean,
      deletePolicy: List[A] => A): Archive[A] = {
    val emptyArchive: Archive[A] = bounded(limit, insertPolicy, deletePolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(envX)(seed)) // ##################################### CHANGES #######################################
  }

  def unboundedNonEmpty[A](envX: Benchmark)(seeds: NonEmptyList[A], insertPolicy: (A, A) => Boolean)
    : Archive[A] = { // ##################################### CHANGES #######################################
    val emptyArchive: Archive[A] = unbounded(insertPolicy)
    seeds.foldLeft(emptyArchive)((archive, seed) => archive.insert(envX)(seed)) // ##################################### CHANGES #######################################
  }

}
